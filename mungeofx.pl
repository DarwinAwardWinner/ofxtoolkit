#!/usr/bin/perl

use Toolkit 'smartenv';
#use Toolkit 'smart4';
use XML::LibXML;
use IO::File;
use XML::Twig;
use Capture::Tiny;
use List::AllUtils qw( maxstr );

main: {
    for my $filename (@ARGV)
    {
        my $input = IO::File->new($filename);
        my $data = read_header_and_body($input);
        $input->close;
        my $body_text = join q(), @{$data->{body}};
        my $munged_body_text = munge_ofx_text($body_text);
        my $output = IO::File->new($filename, 'w');
        $output->print(@{$data->{header}}, $munged_body_text);
        $output->close;
    }
}

# The non-XML-ish header at the top of OFX seems to cause most XML
# processors to choke. This is everything before the "<OFX>" tag. So
# separate out this header before processing the rest as XML, then
# tack the header back on at the end.
sub read_header_and_body {
    alias my $input = $_[0];

    my @header;
    my @body;

    while (<$input>) {
	if ( m{ \s* <OFX> \s* }xsm ) {
            push @body, $_, <$input>;
            last;
	}
	else {
            push @header, $_;
	}
    }

    return {
        header => \@header,
        body => \@body,
    };
}

sub munge_ofx_text {
    alias my $text = $_[0];

    state $date_tags = [
        'DTACCTUP',
        'DTASOF',
        'DTEND',
        'DTPOSTED',
        'DTSERVER',
        'DTSTART',
        'DTUSER',
    ];

    state $stmtrs_tags = [
        'STMTRS',
        'CCSTMTRS',
    ];

    state $transaction_tags = [
        'STMTTRN',
    ];

    # These handlers will munge specific elements
    state $handlers = {
        (map { $_ => \&munge_transaction } @$transaction_tags),
        (map { $_ => \&munge_date } @$date_tags),
        # (map { $_ => \&munge_dtasof } @$stmtrs_tags),
    };

    my $twig = XML::Twig->new(
        twig_handlers => $handlers,
        pretty_print => 'indented',
    );
    $twig->parse($text);
    #$twig->flush;

    return $twig->sprint;
}

sub munge_transaction {
    my $transaction = $_;

    # Retrieve the offending children
    my $name = $transaction->first_child('NAME');
    my $memo = $transaction->first_child('MEMO');

    if ($name and $memo) {
        if (starts_with($memo->text, $name->text)) {
            # If name is a truncated version of memo, then replace it
            # with memo.
            #### Completing name from memo...
            $name->set_text($memo->text);
        }
    }
    elsif ($name) {
        # Missing memo
        #### Copy name to memo...
        $memo = $name->copy;
        $memo->set_tag('MEMO');
        $memo->paste(after => $name);
    }
    elsif ($memo) {
        # Missing name
        #### Copy memo to name...
        $name = $memo->copy;
        $name->set_tag('NAME');
        $name->paste(before => $memo);
    }
    else {
        # Missing both. Not allowed.
        croak "Transaction has no name or memo. The offenting transaction was:\n"
            . $transaction->sprint;
    }
    #### New transaction: $transaction->sprint
}

# Truncate dates to 8 digits: YYYYMMDD, because the stuff after that
# is usually invalid and evil.
sub munge_date {
    my $date = $_;
    $date->set_text(substr($date->trimmed_text,0,8));

}

# Change every DTASOF field to match the latest DTPOSTED of any
# transaction, because the provided DTASOF field is usually a lie.
sub munge_dtasof {
    #### Munging DTASOF...
    my $tranlist = $_[0];
    my $latest_post_date = maxstr map { $_->trimmed_text } $tranlist->get_xpath('.//DTPOSTED');
    #### Latest post date: $latest_post_date
    my @as_of_date_nodes = $tranlist->get_xpath('.//DTASOF');
    for my $node (@as_of_date_nodes) {
        $node->set_text($latest_post_date);
    }
}

sub starts_with {
    alias my ($string, $start) = @_;
    return substr($string, 0, length $start) eq $start;
}
