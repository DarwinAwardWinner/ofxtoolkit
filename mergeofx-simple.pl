#!/usr/bin/perl
use strict;
use warnings;
use v5.10;
use autodie qw{ :all };
use utf8;

use Carp;
use Clone 'clone';
use Data::Alias;
use File::Path qw(make_path);
use File::Spec::Functions qw(catfile);
use File::Util qw(can_write existent);
use Getopt::Euclid;
use IO::File;
use Lingua::EN::Titlecase;
use List::AllUtils qw( all first first_index last_index uniq maxstr );
use Smart::Comments '####';
#use Smart::Comments '###';
use Sort::Maker qw(make_sorter sorter_source);
use Try::Tiny;
use XML::Compare;
use XML::Twig::XPath;

main: {
    ### Begin script...

    #$ARGV{'<ofxfiles>'} or croak "No input files were specified.";

    ### Preparing output directory...

    prepare_output_directory();

    ### Reading input files...

    alias my @input_files = @{$ARGV{'<ofxfiles>'}};
    if (@input_files == 0 and ($ARGV{'--overwrite'} or not $ARGV{'--update'}))
    {
        die "No input files were specified.\n"
    }

    my @existing_ofx_files = map { catfile($ARGV{'--output-directory'},$_) } list_dir($ARGV{'--output-directory'}, '--pattern=\.ofx$');

    if ($ARGV{'--update'})
    {
        # In order to update existing files, we just add the existing
        # files to the input, and then overwrite.
        push @input_files, @existing_ofx_files;
    }



    my @input_ofx = map {
        my $filename = $_;
        #### assert: $filename
        my $ofx = read_header_and_body(IO::File->new($filename, 'r') or croak "Could not open $filename");
        $ofx->{filename} = $filename;
        $ofx->{xml} = parse_ofx_text($ofx->{body});

        # ## check: (get_xpath($ofx->{xml}, 'fi'))[0]->sprint
        ### assert: (get_xpath($ofx->{xml}, 'transaction_list'))[0]->sprint
        ### assert: (get_xpath($ofx->{xml}, 'acctfrom'))[0]->sprint

        # List of hashes with keys = (header, body, xml)
        $ofx;
    } uniq(@input_files);

    ### Grouping by account...

    my @ofx_groups = group_ofx_by_account(@input_ofx);

    ### Merging...

    my @merged_ofx = map { merge_ofx(@$_) } @ofx_groups;

    ### Saving output...

    # For updating, the file names may change, so unlink the old file
    # names. For overwrite, we're just blowing them away. For abort, we
    # won't ever get here. But just to make sure...
    #### assert: not $ARGV{'--abort'}
    if (@existing_ofx_files)
    {
        unlink(@existing_ofx_files);
    }

    foreach my $ofx (@merged_ofx)
    {
        if (! $ARGV{'--dry-run'}) {
            save_ofx_in_directory($ofx,$ARGV{'--output-directory'});
        }
        else {
            say "Would have saved " . gen_ofx_basename($ofx) . " in " . $ARGV{'--output-directory'} . ".";
        }
    }


    exit 0;
}

sub prepare_directory {
    #### assert: all { defined } @_

    my $dir = $_[0];
    existent $dir or make_path $dir;
    croak "Desired directory $dir is not writable."
        if not can_write $dir;
}

sub list_dir {
    #### assert: all { defined } @_

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
            die "Output directory already contains OFX files. Aborting.\n";
        }
        elsif ($ARGV{'--update'} or $ARGV{'--overwrite'}) {
            if ($ARGV{'--update'} and $ARGV{'--overwrite'}) {
                die "You cannot specify both --update and --overwrite options.\n";
            }
            # Do nothing. We'll handle individual files later.
        }
        else {
            # Abort by default
            die "Output directory already contains OFX files. Aborting.\n";
        }
    }
    return 1;
}

# The non-XML-ish header at the top of OFX seems to cause most XML
# processors to choke. This is everything before the opening "<OFX>"
# tag. So separate out this header before processing the rest as XML,
# then tack the header back on as we print it back out to a file.

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
    my $xpath = $common_xpath->{$_[1]} || $_[1]
        or croak "Invalid XPath: $_[1]";

    #### assert: $twig->isa('XML::Twig');

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
    #### assert: get_xpath($twig, 'fi_parent')

    # This one is not mandatory
    # ## check: get_xpath($twig, 'fi')

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

BEGIN {

    sub sort_ofx_by_latest_transaction;
    make_sorter(
        name => 'sort_ofx_by_latest_transaction',
        ascending => 1,
        'GRT', 'closure',
        string => sub {
            return get_latest_transaction_date($_);
        }
    ) or die "make_sorter: $@";

    sub sort_transactions_by_date;
    make_sorter(
        name => 'sort_transactions_by_date',

        # Sort ascending
        ascending => 1,

        # Sort optimized by GRT
        'GRT',

        'closure',
        # Sort keys:
        # First use date posted (DTPOSTED)
        string => {
            closure => 1,
            code => sub {
                ### assert: $_->isa('XML::Twig::Elt')
                ### assert: $_->tag eq 'STMTTRN'
                ### assert: $_->has_child('DTPOSTED')
                ### assert: $_->has_child('FITID')
                # ## $_->sprint

                try {
                    return $_->first_child('DTPOSTED')->trimmed_text;
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
                    return $_->first_child('DTUSER')->trimmed_text;
                } catch {
                    return q();
                }
            },
        },
        # Last use FITID, which must be unique.
        string => {
            closure => 1,
            code => sub {
                try {
                    return $_->first_child('FITID')->trimmed_text;
                } catch {
                    croak "Transaction has no FITID.";
                }
            },
        },
    ) or die "make_sorter: $@";
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
    #### assert: all { defined } @_
    return is_twig_same(
        map {
            try {
                (get_xpath($_, 'acctfrom'))[0];
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
    #### assert: all { defined } @_
    alias my @ungrouped = @_;

    # Shortcut: First ofx definitely gets its own group.
    my @groups = @ungrouped ? [ pop @ungrouped ] : return;
    while (@ungrouped) {
        my $ofx = pop @ungrouped;
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
      # FINDGROUP: for my $ofx_group (@groups) {
      #       if (is_account_same($ofx->{xml}, $ofx_group->[0]->{xml})) {
      #           push @$ofx_group, $ofx;
      #           last FINDGROUP;
      #       }
      #   }
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

sub merge_ofx {
    #### assert: all { $_->{xml}->isa('XML::Twig') } @_

    # Shortcuts for merging a set of zero or one ofx files, where no
    # merging is required.
    if (@_ == 0) {
        return;
    }
    elsif (@_ == 1) {
        return $_[0];
    }

    # Don't change the originals; sort by ledger date
    my @input_ofx = sort_ofx_by_latest_transaction(@_);

    ### Merging: map { gen_ofx_basename($_) } @input_ofx

    #### assert: @input_ofx == @_

    # Extract just the xml
    my @ofx_xml = map { $_->{xml} } @input_ofx;

    #### assert: @ofx_xml == @input_ofx

    # Get all the transactions, without duplicates, sorted by date
    my @all_transactions =  map {
        my $tlist = (get_xpath($_, 'transaction_list'))[0];
        #### assert: $tlist->isa('XML::Twig::Elt')
        my @children = $tlist->children('STMTTRN');
        @children;
    } @ofx_xml;
    my @deduped_transactions = deduplicate_transactions(@all_transactions);
    my @transactions = sort_transactions_by_date(@deduped_transactions);

    # Copy most attributes from the latest one
    my $merged_ofx_xml = clone $ofx_xml[-1];

    # Merge <FI> blocks
    my @fi_blocks = map { get_xpath($_, 'fi') } @ofx_xml;
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
        if (my $existing_fi = (get_xpath($merged_ofx_xml, 'fi'))[0]) {
            $existing_fi->cut;
        }
        my $merged_fi_parent = (get_xpath($merged_ofx_xml, 'fi_parent'))[0];
        $merged_fi->paste(last_child => $merged_fi_parent);
    }

    # Generate merged transaction list
    my $merged_tranlist = (get_xpath($merged_ofx_xml, 'transaction_list'))[0];

    ### assert: $merged_tranlist->can('first_child')
    ### assert: $merged_tranlist->isa('XML::Twig::XPath::Elt')
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
    #### assert: $xml->isa('XML::Twig')

    # Gather information
    my $bank = title_case(
        try {
            (get_xpath($xml, 'fi'))[0]->first_child('ORG')->trimmed_text
        } catch { try {
            (get_xpath($xml, 'acctfrom'))[0]->first_child('BANKID')->trimmed_text
        } catch { '' }}
    );
    # Any bank identifier that doesn't start with a letter should have
    # 'Bank_' prepended.
    if ($bank and ($bank !~ m{^\w})) {
        $bank = "Bank_" . $bank;
    }

    my $acctfrom = (get_xpath($xml, 'acctfrom'))[0];

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

    my $date = substr(get_latest_transaction_date($_[0]),0,8);

    # Generate the name
    my $full_name = q();
    if ($bank) {
        $full_name .= "$bank" . q(_);
    }
    $full_name .= "${type}_${acct}_As_of_${date}";


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

__END__

=head1 NAME

    mergeofx.pl - Merge a set of OFX files into one file per account

=head1 VERSION

version 0.0.1

=head1 USAGE

    mergeofx

=head1 REQUIRED ARGUMENTS

=over

=item --output-directory [=] <outdir> | -o [=] <outdir>

Specify the output directory for the merged OFX files. One file per
account will be created (or updated) in this directory.

=back

=head1 OPTIONS

=over

=item <ofxfiles>

The OFX files to merge. These can be files from multiple accounts.
Each will only be merged with others of the same account.

=for Euclid:
    ofxfiles.type: readable
    repeatable

=item --update[-on-conflict]

=item --overwrite[-on-conflict]

=item --abort[-on-conflict]

If the output directory already has OFX files for some of the accounts
specified in the input files, then these options tell the program what
to do. "Update" means that the new data should be merged into the
existing files in that directory. This is useful for when you download
new statements and need to merge them with your current data.
"Overwrite" means that the old files should be deleted and replaced
with the new data. By default, the program will abort, because this
minimizes the chance of data loss.

As a special case, you can use --update without any input files to
simply re-process the existing ofx files in the output directory. This
would only be useful if you manually edited them and wanted them to be
automatically renamed according to your edits. But you probably
shouldn't be doing that. Shame on you.

=item --dry-run | -n

With this option, no output files will be created or modified. The input files will simply be merged and then the results discarded. You can use this to verify that a set of OFX files is valid and mergeable without errors, before running it for real. Generally, this should not be necessary, becase the output files will not be produced if any errors occur.

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
account. Or maybe it's just floating-point errors. You'll never know.

=head1 COPYRIGHT

Copyright (c) 2010, Ryan Thompson. All Rights Reserved.
This module is free software. It may be used, redistributed
and/or modified under the terms of the Perl Artistic License
(see http://www.perl.com/perl/misc/Artistic.html)
