#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @r8h @immed64 @immed32 @immed16 @immed8 );
use Test::More;
use Log::Any::Adapter 'TAP';

sub test_mov_reg {
	# Generate every combination of to/from registers
	my (@asm, @out);
	for my $src (@r64) {
		for my $dst (@r64) {
			push @asm, "mov $dst, $src";
			push @out, new_writer->mov64_reg_reg($dst, $src)->bytes;
		}
	}
	asm_ok( \@out, \@asm, '64-bit reg-to-reg instructions' );
	done_testing;
}

sub test_mov_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed64) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov64_reg_imm($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov32_reg_imm($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov16_reg_imm($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );
	
	@asm= (); @out= ();
	for my $dst (@r8, @r8h) {
		for my $val (@immed8) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov8_reg_imm($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );

	done_testing;
}

sub test_mov_mem {
	my (@asm, @out);
	for my $reg (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "mov $_[0], $reg" },
			\@out, sub { new_writer->mov64_mem_reg([@_], $reg)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "mov $reg, $_[0]" },
			\@out, sub { new_writer->mov64_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'mov64_mem_*' );
	
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "mov $_[0], $reg" },
			\@out, sub { new_writer->mov32_mem_reg([@_], $reg)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "mov $reg, $_[0]" },
			\@out, sub { new_writer->mov32_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'mov32_mem_*' );

	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "mov $_[0], $reg" },
			\@out, sub { new_writer->mov16_mem_reg([@_], $reg)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "mov $reg, $_[0]" },
			\@out, sub { new_writer->mov16_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'mov16_mem_*' );

	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "mov $_[0], $reg" },
			\@out, sub { new_writer->mov8_mem_reg([@_], $reg)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "mov $reg, $_[0]" },
			\@out, sub { new_writer->mov8_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'mov8_mem_*' );

	done_testing;
}

subtest mov_reg => \&test_mov_reg;
subtest mov_const => \&test_mov_const;
subtest mov_mem => \&test_mov_mem;
done_testing;
