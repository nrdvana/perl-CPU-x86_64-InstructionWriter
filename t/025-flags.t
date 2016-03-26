#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM;
use Test::More;
use Log::Any::Adapter 'TAP';
use CPU::x86_64::InstructionWriter;

sub new_writer { CPU::x86_64::InstructionWriter->new };

my (@asm, @out);

my %alias= (
	clc => [ flag_carry => 0 ],
	cmc => [ flag_carry => -1 ],
	stc => [ flag_carry => 1 ],
	cld => [ flag_direction => 0 ],
	std => [ flag_direction => 1 ],
);
for my $op (keys %alias) {
	my ($method, $arg)= @{ $alias{$op} };
	push @asm, $op."\n".$op;
	push @out, new_writer->$op->bytes . new_writer->$method($arg)->bytes;
}

asm_ok(\@out, \@asm, 'flag modifiers');

done_testing;