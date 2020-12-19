#!/usr/bin/env perl
use 5.28.0;

while (<>) {
    chomp;
    next unless /^\d+:/../^$/;
    next unless $_;
    my ($no, $rule) = split /: /;
    $rule = "(?:$rule)";
    $rule =~ s/\|/|/;
    $rule =~ s/(\d+)/#{rule$1}/g;
    $rule =~ tr/" "//d;
    say "rule$no = [i|$rule|] :: String";
}
