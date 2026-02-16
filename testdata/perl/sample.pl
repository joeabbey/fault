#!/usr/bin/perl
use strict;
use warnings;
use File::Basename;
use Getopt::Long;

package MyApp;

sub new {
    my ($class, %args) = @_;
    return bless \%args, $class;
}

sub process {
    my ($self, $input) = @_;
    my $base = basename($input);
    return $base;
}

sub _validate {
    my ($self, $data) = @_;
    return defined $data && length($data) > 0;
}

1;
