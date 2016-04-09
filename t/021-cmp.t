#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @r8h @immed64 @immed32 @immed16 @immed8 );
use Test::More;
use Log::Any::Adapter 'TAP';

subtest cmp_reg => \&cmp_reg;
sub cmp_reg {
	my (@asm, @out);
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg64, reg64' );
	
	@asm= (); @out= ();
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg32, reg32' );
	
	@asm= (); @out= ();
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg16, reg16' );
	
	@asm= (); @out= ();
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg8, reg8' );
	
	done_testing;
}

subtest cmp_const => \&cmp_const;
sub cmp_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed32) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for my $val (@immed8) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );
}

subtest cmp_mem => \&cmp_mem;
sub cmp_mem {
	my (@asm, @out);
	for my $reg (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp $reg, $_[0]" },
			\@out, sub { new_writer->cmp64_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp $reg, $_[0]" },
			\@out, sub { new_writer->cmp32_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp $reg, $_[0]" },
			\@out, sub { new_writer->cmp16_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp $reg, $_[0]" },
			\@out, sub { new_writer->cmp8_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp8_mem' );

	done_testing;
}

subtest cmp_const_mem => \&cmp_const_mem;
sub cmp_const_mem {
	my (@asm, @out);
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp qword $_[0], $immed" },
			\@out, sub { new_writer->cmp64_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp dword $_[0], $immed" },
			\@out, sub { new_writer->cmp32_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed16) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp word $_[0], $immed" },
			\@out, sub { new_writer->cmp16_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed8) {
		iterate_mem_addr_combos(
			\@asm, sub { "cmp byte $_[0], $immed" },
			\@out, sub { new_writer->cmp8_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'cmp8_const_to_mem' );
	
	done_testing;
}

done_testing;
