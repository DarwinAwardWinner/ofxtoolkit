#!/usr/bin/perl

use Carp;
use File::Path qw(make_path);
use File::Spec::Functions qw(catfile splitpath);
use File::Util qw(can_write existent);
use File::Copy 'move';
use IO::File;
use Toolkit;
{
    # These apparently have to be in this order. They're both source
    # filters, so you can't really complain.
    use Smart::Comments '####';
    use Yada::Yada::Yada;
}
use XML::Twig::XPath;
use Lingua::EN::Titlecase;
use Try::Tiny;

sub read_header_and_body {
    #### assert: all { defined } @_

    alias my $input = $_[0];

    my @header;
    my @body;

    while (<$input>) {
	if ( m/ \s* <OFX> \s* /xsm ) {
            push @body, $_, <$input>;
            last;
	}
	else {
            push @header, $_;
	}
    }

    return {
        header => join('', @header),
        body => join('', @body),
    };
}

sub get_xpath {
    #### assert: all { defined } @_

    # Shortcuts for what I use
    state $common_xpath = {
        transaction_list => # Entire transaction list element
            '/OFX/BANKMSGSRSV1/STMTTRNRS/STMTRS/BANKTRANLIST|/OFX/CREDITCARDMSGSRSV1/CCSTMTTRNRS/CCSTMTRS/BANKTRANLIST',
        acctfrom =>         # Account identity
            '/OFX/BANKMSGSRSV1/STMTTRNRS/STMTRS/BANKACCTFROM|/OFX/CREDITCARDMSGSRSV1/CCSTMTTRNRS/CCSTMTRS/CCACCTFROM',
        # ledger_balance =>   # Account balance
        #     '/OFX/BANKMSGSRSV1/STMTTRNRS/STMTRS/LEDGERBAL|/OFX/CREDITCARDMSGSRSV1/CCSTMTTRNRS/CCSTMTRS/LEDGERBAL',
        fi =>               # Financial institution identity
            '/OFX/SIGNONMSGSRSV1/SONRS/FI',
        fi_parent =>               # Financial institution identity
            '/OFX/SIGNONMSGSRSV1/SONRS',
    };

    #### check: exists $common_xpath->{$_[1]}
    if (not exists $common_xpath->{$_[1]}) {
        #### Non-standard xpath: $_[1]
    }

    my $twig = $_[0];
    my $xpath = $common_xpath->{$_[1]} || $_[1];
    my $index = $_[2] || '';

    return $twig->findnodes($xpath);
}

# Takes a string or handle containing everything from <OFX> to </OFX>
# inclusive, and returns an XML::Twig::XPath of the parse tree.
sub parse_ofx_text {
    alias my $input = $_[0];
    my $twig = XML::Twig::XPath->new(
        pretty_print => 'indented',
    );
    $twig->parse($input);
    #### assert: get_xpath($twig, 'transaction_list')
    #### assert: get_xpath($twig, 'acctfrom')
    # ### assert: get_xpath($twig, 'ledger_balance')
    #### assert: get_xpath($twig, 'fi_parent')

    # This one is not mandatory
    ### check: get_xpath($twig, 'fi')

    return $twig;
}

sub get_latest_transaction_date {
    my $ofx = $_[0];
    my $twig = $ofx->{xml};
    my $tlist = (get_xpath($twig, 'transaction_list'))[0]
        or croak "OFX has no transaction list: " . $_->{filename};
    my @transactions = $tlist->children('STMTTRN');
    my $latest_date = maxstr (
        map {
            $_->first_child('DTPOSTED')->trimmed_text
        } @transactions
    ) // '';
    return $latest_date;
}

# Generate a filename for an ofx
sub gen_ofx_basename {
    my $xml = $_[0]->{xml};

    # Gather information
    my $bank = title_case(
        try {
            (get_xpath($xml,'fi'))[0]->first_child('ORG')->trimmed_text
        } catch { try {
            (get_xpath($xml,'acctfrom'))[0]->first_child('BANKID')->trimmed_text
        } catch { '' }}
    );
    # Any bank identifier that doesn't start with a letter should have
    # 'Bank_' prepended.
    if ($bank and ($bank !~ m{^\w})) {
        $bank = "Bank_" . $bank;
    }

    my $acctfrom = (get_xpath($xml,'acctfrom'))[0];

    my $acct = try {
        $acctfrom->first_child('ACCTID')->trimmed_text;
    } catch { croak "Missing account; cannot generate filename for ofx." };

    my $type = title_case (try {
        $acctfrom->first_child('ACCTTYPE')->trimmed_text;
    } catch {
        if ($acctfrom->tag eq 'CCACCTFROM') {
            'Credit Card';
        }
        else {
            'Account';
        }
    });

    my $transaction_list = (get_xpath($xml,'transaction_list'))[0];
    my $start_date = $transaction_list->first_child('DTSTART')->trimmed_text;
    my $dtend = $transaction_list->first_child('DTEND')->trimmed_text;
    # my $dtledger = (get_xpath($xml,'ledger_balance'))[0]->first_child('DTASOF')->trimmed_text;
    my $dtledger = get_latest_transaction_date($_[0]);
    my $end_date = maxstr ($dtend, $dtledger);

    # Only keep the leading digits
    $start_date =~ s/[^0-9].*//;
    $end_date =~ s/[^0-9].*//;

    # Generate the name
    my $full_name = q();
    if ($bank) {
        $full_name .= "$bank" . q(_);
    }
    $full_name .= $type . q(_) . $acct;

    $full_name .= q(_Date_) . $start_date . q(-) . $end_date;

    $full_name =~ s/ /_/g;

    return $full_name;
}

sub title_case {
    return Lingua::EN::Titlecase->new($_[0])->title;
}

# sub save_ofx_in_directory {
#     my ($ofx, $dir) = @_;
#     my $basename = gen_ofx_basename($ofx);
#     my $extension = '.ofx';
#     my $filename = catfile($dir, $basename . $extension);

#     my $output = IO::File->new($filename, 'w');
#     $output->print($ofx->{header} . $ofx->{body});
#     $output->close;
#     return 1;
# }

my @input_ofx = map {
    my $filename = $_;
    #### assert: $filename
    my $ofx = read_header_and_body(IO::File->new($filename, 'r') or croak "Could not open $filename");
    $ofx->{filename} = $filename;
    $ofx->{xml} = parse_ofx_text($ofx->{body});

    ### check: (get_xpath($ofx->{xml},'fi'))[0]->sprint
    #### assert: (get_xpath($ofx->{xml},'transaction_list'))[0]->sprint
    #### assert: (get_xpath($ofx->{xml},'acctfrom'))[0]->sprint
    # ### assert: (get_xpath($ofx->{xml},'ledger_balance'))[0]->sprint

    $ofx;
} @ARGV;

foreach my $ofx (@input_ofx) {

    my ($volume,$dir,$infile) = splitpath( $ofx->{filename} );
    $dir = $volume . $dir;
    my $outfile = gen_ofx_basename($ofx) . ".ofx";

    if ($dir) {
        $infile = catfile($dir, $infile);
        $outfile = catfile($dir, $outfile);
    }

    ### Input file: $infile
    ### Output file: $outfile

    move($infile,$outfile)
        or die "Failed to move $infile to $outfile";
}
