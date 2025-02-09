#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r128 @r64 );
use Test::More;

subtest movq_xreg_xreg => \&movq_xreg_xreg;
sub movq_xreg_xreg {
	my (@asm, @out);
	for my $r1 (@r128) {
		for my $r2 (@r128) {
			push @asm, "movq $r1, $r2";
			push @out, new_writer->movq_xreg_xreg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'movq_xreg_xreg' );
	
	done_testing;
}

subtest movq_xreg_reg => \&movq_xreg_reg;
sub movq_xreg_reg {
	my (@asm, @out);
	for my $r1 (@r128) {
		for my $r2 (@r64) {
			push @asm, "movq $r1, $r2";
			push @out, new_writer->movq_xreg_reg($r1, $r2)->bytes;
			push @asm, "movq $r2, $r1";
			push @out, new_writer->movq_reg_xreg($r2, $r1)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'movq_xreg_reg' );
	
	done_testing;
}

subtest movq_xreg_mem => \&movq_xreg_mem;
sub movq_xreg_mem {
	my (@asm, @out);
	for my $r1 (@r128) {
		iterate_mem_addr_combos(
			\@asm, sub { "movq $_[0], $r1" },
			\@out, sub { new_writer->movq_mem_xreg([@_], $r1)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "movq $r1, $_[0]" },
			\@out, sub { new_writer->movq_xreg_mem($r1, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'movq_xreg_reg' );
	
	done_testing;
}

done_testing;
