#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @immed32 @immed16 @immed8 );
use Test::More;
use Log::Any::Adapter 'TAP';

subtest xor_reg => \&xor_reg;
sub xor_reg {
	my (@asm, @out);
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "xor $r1, $r2";
			push @out, new_writer->xor64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor64_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "xor $r1, $r2";
			push @out, new_writer->xor32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor32_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "xor $r1, $r2";
			push @out, new_writer->xor16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor16_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "xor $r1, $r2";
			push @out, new_writer->xor8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor8_reg' );
	
	done_testing;
}

subtest xor_const => \&xor_const;
sub xor_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed32) {
			push @asm, "xor $dst, $val";
			push @out, new_writer->xor64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "xor $dst, $val";
			push @out, new_writer->xor32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "xor $dst, $val";
			push @out, new_writer->xor16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for my $val (@immed8) {
			push @asm, "xor $dst, $val";
			push @out, new_writer->xor8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'xor8_const' );
}

subtest xor_mem => \&xor_mem;
sub xor_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor qword $dst, $_[0]" },
			\@out, sub { new_writer->xor64_mem($dst, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "xor qword $_[0], $dst" },
			\@out, sub { new_writer->xor64_to_mem($dst, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor dword $reg, $_[0]" },
			\@out, sub { new_writer->xor32_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "xor dword $_[0], $reg" },
			\@out, sub { new_writer->xor32_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor word $reg, $_[0]" },
			\@out, sub { new_writer->xor16_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "xor word $_[0], $reg" },
			\@out, sub { new_writer->xor16_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor byte $reg, $_[0]" },
			\@out, sub { new_writer->xor8_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "xor byte $_[0], $reg" },
			\@out, sub { new_writer->xor8_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor8_mem' );

	done_testing;
}

subtest xor_const_mem => \&xor_const_mem;
sub xor_const_mem {
	my (@asm, @out);
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor qword $_[0], $immed" },
			\@out, sub { new_writer->xor64_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor dword $_[0], $immed" },
			\@out, sub { new_writer->xor32_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed16) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor word $_[0], $immed" },
			\@out, sub { new_writer->xor16_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed8) {
		iterate_mem_addr_combos(
			\@asm, sub { "xor byte $_[0], $immed" },
			\@out, sub { new_writer->xor8_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'xor8_const_to_mem' );
	
	done_testing;
}

done_testing;
