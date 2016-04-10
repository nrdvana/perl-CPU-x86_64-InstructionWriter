#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @immed32 @immed16 @immed8 );
use Test::More;
use Log::Any::Adapter 'TAP';

subtest and_reg => \&and_reg;
sub and_reg {
	my (@asm, @out);
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "and $r1, $r2";
			push @out, new_writer->and64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'and reg64, reg64' );
	
	@asm= (); @out= ();
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "and $r1, $r2";
			push @out, new_writer->and32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'and reg32, reg32' );
	
	@asm= (); @out= ();
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "and $r1, $r2";
			push @out, new_writer->and16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'and reg16, reg16' );
	
	@asm= (); @out= ();
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "and $r1, $r2";
			push @out, new_writer->and8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'and reg8, reg8' );
	
	done_testing;
}

subtest and_const => \&and_const;
sub and_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed32) {
			push @asm, "and $dst, $val";
			push @out, new_writer->and64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "and $dst, $val";
			push @out, new_writer->and32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "and $dst, $val";
			push @out, new_writer->and16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for my $val (@immed8) {
			push @asm, "and $dst, $val";
			push @out, new_writer->and8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );
}

subtest and_mem => \&and_mem;
sub and_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "and qword $dst, $_[0]" },
			\@out, sub { new_writer->and64_mem($dst, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "and qword $_[0], $dst" },
			\@out, sub { new_writer->and64_to_mem($dst, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "and dword $reg, $_[0]" },
			\@out, sub { new_writer->and32_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "and dword $_[0], $reg" },
			\@out, sub { new_writer->and32_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "and word $reg, $_[0]" },
			\@out, sub { new_writer->and16_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "and word $_[0], $reg" },
			\@out, sub { new_writer->and16_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "and byte $reg, $_[0]" },
			\@out, sub { new_writer->and8_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "and byte $_[0], $reg" },
			\@out, sub { new_writer->and8_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and8_mem' );

	done_testing;
}

subtest and_const_mem => \&and_const_mem;
sub and_const_mem {
	my (@asm, @out);
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "and qword $_[0], $immed" },
			\@out, sub { new_writer->and64_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "and dword $_[0], $immed" },
			\@out, sub { new_writer->and32_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed16) {
		iterate_mem_addr_combos(
			\@asm, sub { "and word $_[0], $immed" },
			\@out, sub { new_writer->and16_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed8) {
		iterate_mem_addr_combos(
			\@asm, sub { "and byte $_[0], $immed" },
			\@out, sub { new_writer->and8_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'and8_const_to_mem' );
	
	done_testing;
}

done_testing;
