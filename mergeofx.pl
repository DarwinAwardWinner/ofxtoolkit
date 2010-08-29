#!/usr/bin/perl

use Carp;
use Clone 'clone';
use File::Path qw(make_path);
use File::Spec::Functions qw(catfile);
use File::Util qw(can_write existent);
use IO::File;
use Memoize;
use Toolkit qw(euclid);
{
    # These apparently have to be in this order. They're both source
    # filters, so you can't really complain.
    use Smart::Comments '####';
    use Yada::Yada::Yada;
}
use XML::Compare;
use XML::Twig;
use XML::Twig::XPath;
use Lingua::EN::Titlecase;
use Sort::Maker qw(make_sorter sorter_source);
use Try::Tiny;
use Scalar::Util qw(blessed);
sub prepare_directory {
    #### assert: all { defined } @_

    my $dir = $_[0];
    existent $dir or make_path $dir;
    croak "Desired directory $dir is not writable."
        if not can_write $dir;
}

sub list_dir {
    ### assert: all { defined } @_

    state $f = File::Util->new();
    $f->list_dir(@_);
}

sub prepare_output_directory {
    alias my $outdir = $ARGV{'--output-directory'};
    prepare_directory $outdir;


    my @existing_ofx_files = list_dir($outdir, '--pattern=\.ofx$');
    if (@existing_ofx_files) {
        if ($ARGV{'--abort'}) {
            # Abort option takes precedence
            croak "Output directory already contains OFX files. Aborting.";
        }
        elsif ($ARGV{'--update'} or $ARGV{'--overwrite'}) {
            # Do nothing. We'll handle individual files later.
        }
        else {
            # Abort by default
            croak "Output directory already contains OFX files. Aborting.";
        }
    }
    return 1;
}

# The non-XML-ish header at the top of OFX seems to cause most XML
# processors to choke. This is everything before the "<OFX>" tag. So
# separate out this header before processing the rest as XML, then
# tack the header back on as we print it back out to a file.

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
        ledger_balance =>   # Account balance
            '/OFX/BANKMSGSRSV1/STMTTRNRS/STMTRS/LEDGERBAL|/OFX/CREDITCARDMSGSRSV1/CCSTMTTRNRS/CCSTMTRS/LEDGERBAL',
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
    #### assert: get_xpath($twig, 'ledger_balance')
    #### assert: get_xpath($twig, 'fi_parent')

    # This one is not mandatory
    ### check: get_xpath($twig, 'fi')

    return $twig;
}


BEGIN {
    sub sort_ofx_by_ledger_date;
    make_sorter(
        name => 'sort_ofx_by_ledger_date',

        # Sort ascending
        ascending => 1,

        # Sort optimized by GRT
        'GRT',

        # Need a closure to access get_xpath()
        'closure',
        # Sort by ledger date
        string => {
            code => sub {
                my $xml = $_->{xml};
                my $lbal = (get_xpath($xml,'ledger_balance'))[0]
                    or croak "OFX has no ledger balance: " . $_->{filename};
                my $ldate = $lbal->first_child('DTASOF')
                    or croak "Ledger balance has no timestamp: " . $_->{filename}
                        or 1;
                # ## Key: $ldate->trimmed_text
                return $ldate->trimmed_text;
            },
        }
    ) or die "make_sorter: $@";
    ### ledger sorter source: sorter_source(\&sort_ofx_by_ledger_date)

    make_sorter(
        name => 'sort_transactions_by_date',

        # Sort ascending
        ascending => 1,

        # Sort optimized by GRT
        'GRT',


        # Sort keys:
        # First use date posted (DTPOSTED)
        string => {
            closure => 1,
            code => sub {
                ### assert: $_->isa('XML::Twig::Elt')
                ### assert: $_->tag eq 'STMTTRN'
                ### assert: $_->has_child('DTPOSTED')
                ### assert: $_->has_child('FITID')
                ### $_->sprint

                try {
                    $_->first_child('DTPOSTED')->trimmed_text;
                } catch {
                    croak "Transaction has no post date.";
                }
            },
        },
        # Then use DTUSER if available
        string => {
            closure => 1,
            code => sub {
                try {
                    $_->first_child('DTUSER')->trimmed_text;
                } catch {
                    q();
                }
            },
        },
        # Last use FITID, which must be unique.
        string => {
            closure => 1,
            code => sub {
                try {
                    $_->first_child('FITID')->trimmed_text;
                } catch {
                    croak "Transaction has no FITID.";
                }
            },
        },
    ) or die "make_sorter: $@";
    ### transaction sorter source: sorter_source(\&sort_transactions_by_date)
}

# Takes two twigs and returns true if they are semantically the same,
# false otherwise.
sub is_twig_same {
    ### assert: all { defined } @_
    return XML::Compare::is_same(
        map { $_->sprint } @_[0,1]
    );
}

# Takes two ofx twigs and returns true if they come from the same
# account (i.e. their BANKACCTFROM or CCACCTFROM sections are
# identical).
sub is_account_same {
    ### assert: all { defined } @_
    my @xpath1 = get_xpath($_[0],'acctfrom');

return is_twig_same(
        map {
            try {
                (get_xpath($_,'acctfrom'))[0];
            } catch {
                croak "OFX file has no account.\n$_"
            };
        } @_[0,1]
    );
}
#memoize('is_account_same');

# Takes a list of ofx data structures and groups them into a list of
# lists, so that the items in each list all correspond to a single
# account.
sub group_ofx_by_account {
    ### assert: all { defined } @_

    # Shortcut: First ofx definitely gets its own group.
    my @groups = @_ ? [ pop @_ ] : return;
    while (@_) {
        my $ofx = pop;
        my $group = first {
            is_account_same($ofx->{xml}, $_->[0]->{xml})
        } @groups;
        if ($group) {
            # If we found a group, then join it.
            push @$group, $ofx;
        }
        else {
            # Otherwise, start a new group.
            push @groups, [ $ofx ];
        }
      FINDGROUP: for my $ofx_group (@groups) {
            if (is_account_same($ofx->{xml}, $ofx_group->[0]->{xml})) {
                push @$ofx_group, $ofx;
                last FINDGROUP;
            }
        }
    }
    return @groups;
}

# This takes a list of transaction twigs and removes any with
# duplicate FITIDs, and returns the result.
sub deduplicate_transactions {
    ### assert: all { defined } @_
    ### assert: all { $_->isa('XML::Twig::Elt') } @_;

    # The usual to-hash-and-back trick.
    return values %{{map { $_->first_child('FITID')->trimmed_text => $_ } @_}};
}

# Takes lists of ledger balance twigs and transaction twigs, each in
# sorted order, and verifies that the balance change during each
# inter-balance period matches the sum of transaction amounts during
# that period. Returns false if not.
sub reconcile_ledgers {
    ### assert: all { defined } @_
    my @ledger_balances = @{$_[0]};
    my @transactions = @{$_[1]};

    # Need at least two balances to do any meaningful verification, so
    # if there's fewer, we have to hope that everything's OK.
    return 1 if @ledger_balances < 2;

    my $first_ledger = shift @ledger_balances;
    my @ledger_dates = ( $first_ledger->first_child('DTASOF')->trimmed_text );
    my @ledger_bals = ( $first_ledger->first_child('BALAMT')->trimmed_text );

    for my $led (@ledger_balances) {
        my $date = $led->first_child('DTASOF')->trimmed_text;
        my $bal = $led->first_child('BALAMT')->trimmed_text;
        # If dates are equal, it's a duplicate, so verify that the
        # balance matches, and then skip it.
        if ($date eq $ledger_dates[-1]) {
            if ($bal != $ledger_bals[-1]) {
                # This would mean that two different balances were
                # reported at the same time. Not good.
                return;
            }
        }
        else {
            # Add non-duplicate
            push @ledger_dates, $date;
            push @ledger_bals, $bal;
        }
    }

    # Again, need two discrete time points, or else there's nothing to
    # verify.
    return 1 if @ledger_dates < 2;

    # Calculate the change in balance (delta) between each pair of
    # consecutive ledger balance dates.
    my @ledger_balance_deltas = map {
        $ledger_bals[$_] - $ledger_bals[$_-1]
    } 1..$#ledger_bals;

    # Now subtract each transaction amount from the appropriate ledger
    # delta. After this, if all the deltas are zero, then we are
    # reconciled. Note that only transactions that occurred between the
    # first and last ledger dates can be reconciled. The others are
    # discarded silently, since they have nothing to reconcile against.
    my $period = -1;
  TRANSACTION: for my $t (@transactions) {
        my $t_date = $t->first_child('DTPOSTED')->trimmed_text;
      FIND_PERIOD: while ($t_date gt $ledger_dates[$period+1]) {
            $period++;
            if ($period >= $#ledger_dates) {
                last TRANSACTION;
            }
        }
        if ($period >= 0) {
            my $t_amt = $t->first_child('TRNAMT')->trimmed_text;
            ### $t_date
            ### $t_amt
            ### $period
            $ledger_balance_deltas[$period] -= $t_amt;
        }


    }


    ### @ledger_bals
    ### @ledger_dates
    ### @transaction_amts
    ### @transaction_dates
    ### @ledger_balance_deltas

    #### assert: @ledger_bals == @ledger_dates
    #### assert: @ledger_balance_deltas == @ledger_bals - 1

    # Now check for non-zero deltas
    state $floating_point_fuzz = 0.0001;
    return all { $_ < $floating_point_fuzz } @ledger_balance_deltas;
}

sub merge_ofx {
    #### assert: all { $_->{xml}->isa('XML::Twig') } @_

    # Shortcuts for zero and one
    if (@_ == 0) {
        return;
    }
    elsif (@_ == 1) {
        return $_[0];
    }

    # Don't change the originals; sort by ledger date
    my @input_ofx = sort_ofx_by_ledger_date(@_);

    #### assert: @input_ofx == @_
    local $_ = undef;

    # Extract just the xml
    my @ofx_xml = map { $_->{xml} } @input_ofx;

    ### assert: @ofx_xml == @input_ofx

    # Get all the transactions, without duplicates, sorted by date
    my @all_transactions =  map {
        my $tlist = (get_xpath($_,'transaction_list'))[0];
        #### assert: $tlist->isa('XML::Twig::Elt')
        my @children = $tlist->children('STMTTRN');
        @children;
    } @ofx_xml;
    my @deduped_transactions = deduplicate_transactions(@all_transactions);
    my @transactions = sort_transactions_by_date(@deduped_transactions);

    # Get all the ledger balances, sorted by date
    my @ledger_balances = map {
        (get_xpath($_,'ledger_balance'))[0];
    } @ofx_xml;

    reconcile_ledgers(\@ledger_balances, \@transactions)
        or croak "Ledger balances do not agree with transactions.";

    # Copy most attributes from the latest one
    my $merged_ofx_xml = clone $ofx_xml[-1];

    # Merge <FI> blocks
    my @fi_blocks = map { get_xpath($_,'fi') } @ofx_xml;
    if (@fi_blocks) {
        # Start with the latest <FI> block, and then fill in missing
        # desired fields from previous ones if possible.
        my $merged_fi = (pop @fi_blocks)->copy;
        # Child tags that we want
        my @want_tags = qw( ORG FID );
        for my $wanted (@want_tags) {
            if (not $merged_fi->has_child($wanted)) {
                my $found = lastval { $_->first_child($wanted) } @fi_blocks;
                if ($found) {
                    $found->copy->paste(last_child => $merged_fi);
                }
            }
        }

        # Now remove the existing FI block from the merged xml and
        # replace it with the merged FI block
        if (my $existing_fi = (get_xpath($merged_ofx_xml,'fi'))[0]) {
            $existing_fi->cut;
        }
        my $merged_fi_parent = (get_xpath($merged_ofx_xml,'fi_parent'))[0];
        $merged_fi->paste(last_child => $merged_fi_parent);
    }

    # Generate merged transaction list
    my $merged_tranlist = (get_xpath($merged_ofx_xml,'transaction_list'))[0];

    ### assert: $merged_tranlist->can('first_child')
    ### Class: ref $merged_tranlist
    # Set date range based on first and last transaction
    my $dtstart =         $transactions[0]->first_child('DTPOSTED')->trimmed_text;

    $merged_tranlist->first_child('DTSTART')->set_text(
        $dtstart
#        $transactions[0]->first_child('DTPOSTED')->trimmed_text
    );
    $merged_tranlist->first_child('DTEND')->set_text(
        $transactions[-1]->first_child('DTPOSTED')->trimmed_text
    );

    # Remove all existing transaction from the transaction list
    $merged_tranlist->cut_children('STMTTRN');

    # Now copy-paste all the transactions, in order
    for my $t (@transactions) {
        $t->copy->paste(last_child => $merged_tranlist);
    }

    my $merged_ofx = {
        header => $input_ofx[-1]->{header},
        xml => $merged_ofx_xml,
        body => $merged_ofx_xml->sprint,
    };

    return $merged_ofx;
}

sub title_case {
    return Lingua::EN::Titlecase->new($_[0])->title;
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

    # Generate the name
    my $full_name = q();
    if ($bank) {
        $full_name .= "$bank" . q(_);
    }
    $full_name .= $type . q(_) . $acct;

    $full_name =~ s/ /_/g;

    return $full_name;
}

sub save_ofx_in_directory {
    my ($ofx, $dir) = @_;
    my $basename = gen_ofx_basename($ofx);
    my $extension = '.ofx';
    my $filename = catfile($dir, $basename . $extension);

    my $output = IO::File->new($filename, 'w');
    $output->print($ofx->{header} . $ofx->{body});
    $output->close;
    return 1;
}


### Begin script...

$ARGV{'--files'} or croak "No input files were specified.";

### Preparing output directory...

prepare_output_directory();

### Reading input files...

alias my @input_files = @{$ARGV{'--files'}};
if ($ARGV{'--update'}) {
    # In order to update existing files, we just add the existing
    # files to the input, and then overwrite.
    my @existing_ofx_files = map { catfile($ARGV{'--output-directory'},$_) } list_dir($ARGV{'--output-directory'}, '--pattern=\.ofx$');
    push @input_files, @existing_ofx_files;
}

my @input_ofx = map {
    alias my $filename = $_;
    #### assert: $filename
    my $ofx = read_header_and_body(IO::File->new($filename, 'r') or croak "Could not open $filename");
    $ofx->{filename} = $filename;
    $ofx->{xml} = parse_ofx_text($ofx->{body});

    ### assert: (get_xpath($ofx->{xml},'fi'))[0]->sprint
    ### assert: (get_xpath($ofx->{xml},'transaction_list'))[0]->sprint
    ### assert: (get_xpath($ofx->{xml},'acctfrom'))[0]->sprint
    ### assert: (get_xpath($ofx->{xml},'ledger_balance'))[0]->sprint

    # List of hashes with keys = (header, body, xml)
    $ofx;
} @input_files;

### Grouping by account...

my @ofx_groups = group_ofx_by_account(@input_ofx);

### Merging...

my @merged_ofx = map { merge_ofx(@$_) } @ofx_groups;

### Saving output...

foreach my $ofx (@merged_ofx) {
    save_ofx_in_directory($ofx,$ARGV{'--output-directory'});
}

exit 0;
__END__

=head1 NAME

    mergeofx.pl - Merge a set of OFX files into one file per account

=head1 VERSION

version 0.0.1

=head1 USAGE

    mergeofx

=head1 OPTIONS

=over

=item --output-directory [=] <outdir> | -o [=] <outdir>

Specify the output directory for the merged OFX files. One file per
account will be created (or updated) in this directory.

=item --update[-on-conflict]

=item --overwrite[-on-conflict]

=item --abort[-on-conflict]

If the output directory already has OFX files for some of the accounts
specified in the input files, then these options tell the program what
to do. "Update" means that the new data should be merged into the
existing files in that directory. This is useful for when you download
new statements and need to merge them with your current data.
"Overwrite" means that the old files should be deleted and replaced
with the new data. By default, the program will abort, because this minimizes the chance of data loss.

=item --files <ofxfiles>... | -f <ofxfiles>...

The OFX files to merge. These can be files from multiple accounts.
Each will only be merged with others of the same account. This must be
last on the command line.

=for Euclid:
    ofxfiles.type: readable

=item --version

=item --usage

=item --help

=item --man

Print the usual program information

=back

=head1 AUTHOR

Ryan Thompson (rct@thompsonclan.org)

=head1 BUGS

This program skims a fraction of a cent off of each transaction that
it processes, and deposits those fractions of cents into my bank
account.

=head1 COPYRIGHT

Copyright (c) 2010, Ryan Thompson. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the terms of the Perl Artistic License
(see http://www.perl.com/perl/misc/Artistic.html)
