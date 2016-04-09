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
			push @out, new_writer->mov64_reg($dst, $src)->bytes;
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
			push @out, new_writer->mov64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );
	
	@asm= (); @out= ();
	for my $dst (@r8, @r8h) {
		for my $val (@immed8) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );

	done_testing;
}

sub test_mov_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		for my $src (@r64) {
			push @asm, "mov $dst, [$src]";
			push @out, new_writer->mov64_mem($dst, $src)->bytes;
			push @asm, "mov [$dst], $src";
			push @out, new_writer->mov64_to_mem($src, $dst)->bytes;
			for my $ofs (@immed32) {
				push @asm, "mov $dst, [$src+$ofs]";
				push @out, new_writer->mov64_mem($dst, $src, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					push @asm, "mov $dst, [$src+$ridx*$mul]";
					push @out, new_writer->mov64_mem($dst, $src, undef, $ridx, $mul)->bytes;
					for my $ofs (@immed32) {
						push @asm, "mov $dst, [$src+$ofs+$ridx*$mul]";
						push @out, new_writer->mov64_mem($dst, $src, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, '64-bit mov load/store and variations' );
	
	done_testing;
}

subtest mov_reg => \&test_mov_reg;
subtest mov_const => \&test_mov_const;
subtest mov_mem => \&test_mov_mem;
done_testing;
