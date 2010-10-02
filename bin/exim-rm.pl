#!/usr/bin/perl

my $exim = "exim" ;

open(QUEUE, "$exim -bpu | grep frozen |") or die "rmfrozen: 
can't open Exim mail queue";

while (<QUEUE>) {
    my $in = $_;
    $in = ~s/^\s+//;                # hack off leading spaces
    my ($age, $size, $id, $brackets, $from, $stuff1, $stuff2, $stuff3) 
        = split /\s+/;
    print "removing message: $id  age: $age\n";
    system("$exim -Mrm $id");
}

close(QUEUE)
