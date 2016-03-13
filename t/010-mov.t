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

my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
my @r32= qw( eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d );
my @r16= qw( ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w );
my @r8=  qw( al cl dl bl spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b );
my @r8h= qw( ah ch dh bh );

sub test_mov_reg {
	# Generate every combination of to/from registers
	my @asm;
	my @out;
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
	my @asm;
	my @out;
	for my $dst (@r64) {
		for (my $bits= 0; $bits < 64; $bits++) {
			push @asm, "mov $dst, ".(1 << $bits);
			push @out, new_writer->mov64_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		no warnings 'portable';
		for my $val (0xFF, 0x7F, 0xFFFF, 0x7FFF, 0xFFFFFFFF, 0x7FFFFFFF, -1, 0x7FFFFFFFFFFFFFFF) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= ();
	@out= ();
	for my $dst (@r32) {
		for (my $bits= 0; $bits < 32; $bits++) {
			push @asm, "mov $dst, ".(1 << $bits);
			push @out, new_writer->mov32_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		for my $val (0xFF, 0x7F, 0xFFFF, 0x7FFF, -1, 0x7FFFFFFF) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= ();
	@out= ();
	for my $dst (@r16) {
		for (my $bits= 0; $bits < 16; $bits++) {
			push @asm, "mov $dst, ".(1 << $bits);
			push @out, new_writer->mov16_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		for my $val (0xFF, 0x7F, -1, 0x7FFF) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );
	
	@asm= ();
	@out= ();
	for my $dst (@r8, @r8h) {
		for (my $bits= 0; $bits < 16; $bits++) {
			push @asm, "mov $dst, ".(1 << $bits);
			push @out, new_writer->mov8_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		for my $val (-1, 0x7F) {
			push @asm, "mov $dst, $val";
			push @out, new_writer->mov8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );

	done_testing;
}

sub test_mov_mem {
	my @asm;
	my @out;
	for my $dst (@r64) {
		for my $src (@r64) {
			push @asm, "mov $dst, [$src]";
			push @out, new_writer->mov64_mem($dst, $src)->bytes;
			push @asm, "mov [$dst], $src";
			push @out, new_writer->mov64_to_mem($src, $dst)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "mov $dst, [$src+$ofs]";
				push @out, new_writer->mov64_mem($dst, $src, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					push @asm, "mov $dst, [$src+$ridx*$mul]";
					push @out, new_writer->mov64_mem($dst, $src, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
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
