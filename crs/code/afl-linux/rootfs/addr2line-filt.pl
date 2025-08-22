#!/usr/bin/env perl
# Runs addr2line but replace addresses inline, displaying it nicely in the
# context you see the address
use v5.26;

while(<STDIN>) {
        my $line = $_;
        while ($line =~ /(0x[0-9a-fA-F]+)/g) {
                my $name = `addr2line -e $ARGV[0] $1`;
                chomp $name;
                $_ =~ s/$1/$name/e;
        }
        print $_;
}
