#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @immed32 @immed16 @immed8 );
use Test::More;
use Log::Any::Adapter 'TAP';

subtest or_reg => \&or_reg;
sub or_reg {
	my (@asm, @out);
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "or $r1, $r2";
			push @out, new_writer->or64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or64_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "or $r1, $r2";
			push @out, new_writer->or32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or32_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "or $r1, $r2";
			push @out, new_writer->or16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or16_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "or $r1, $r2";
			push @out, new_writer->or8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or8_reg' );
	
	done_testing;
}

subtest or_const => \&or_const;
sub or_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed32) {
			push @asm, "or $dst, $val";
			push @out, new_writer->or64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "or $dst, $val";
			push @out, new_writer->or32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "or $dst, $val";
			push @out, new_writer->or16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for my $val (@immed8) {
			push @asm, "or $dst, $val";
			push @out, new_writer->or8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'or8_const' );
}

subtest or_mem => \&or_mem;
sub or_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "or qword $dst, $_[0]" },
			\@out, sub { new_writer->or64_mem($dst, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "or qword $_[0], $dst" },
			\@out, sub { new_writer->or64_to_mem($dst, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "or dword $reg, $_[0]" },
			\@out, sub { new_writer->or32_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "or dword $_[0], $reg" },
			\@out, sub { new_writer->or32_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "or word $reg, $_[0]" },
			\@out, sub { new_writer->or16_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "or word $_[0], $reg" },
			\@out, sub { new_writer->or16_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "or byte $reg, $_[0]" },
			\@out, sub { new_writer->or8_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "or byte $_[0], $reg" },
			\@out, sub { new_writer->or8_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or8_mem' );

	done_testing;
}

subtest or_const_mem => \&or_const_mem;
sub or_const_mem {
	my (@asm, @out);
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "or qword $_[0], $immed" },
			\@out, sub { new_writer->or64_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "or dword $_[0], $immed" },
			\@out, sub { new_writer->or32_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed16) {
		iterate_mem_addr_combos(
			\@asm, sub { "or word $_[0], $immed" },
			\@out, sub { new_writer->or16_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed8) {
		iterate_mem_addr_combos(
			\@asm, sub { "or byte $_[0], $immed" },
			\@out, sub { new_writer->or8_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'or8_const_to_mem' );
	
	done_testing;
}

done_testing;
