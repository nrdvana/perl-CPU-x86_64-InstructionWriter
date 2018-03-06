#! /usr/bin/env perl
use strict;
use warnings;
no warnings 'portable';
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

sub test_mov_ax_addr {
	my (@asm, @out);
	push @asm, "mov RAX, [qword 0xFF00FF00FF00FF00]";
	push @asm, "mov [qword 0xFF00FF00FF00FF00], RAX";
	push @out, new_writer->mov64_rax_memaddr(0xFF00FF00FF00FF00)->bytes;
	push @out, new_writer->mov64_memaddr_rax(0xFF00FF00FF00FF00)->bytes;
	asm_ok( \@out, \@asm, 'mov64_memaddr' );
	
	push @asm, "mov EAX, [qword 0xFF00FF00FF00FF00]";
	push @asm, "mov [qword 0xFF00FF00FF00FF00], EAX";
	push @out, new_writer->mov32_eax_memaddr(0xFF00FF00FF00FF00)->bytes;
	push @out, new_writer->mov32_memaddr_eax(0xFF00FF00FF00FF00)->bytes;
	asm_ok( \@out, \@asm, 'mov32_memaddr' );
	
	push @asm, "mov AX, [qword 0xFF00FF00FF00FF00]";
	push @asm, "mov [qword 0xFF00FF00FF00FF00], AX";
	push @out, new_writer->mov16_ax_memaddr(0xFF00FF00FF00FF00)->bytes;
	push @out, new_writer->mov16_memaddr_ax(0xFF00FF00FF00FF00)->bytes;
	asm_ok( \@out, \@asm, 'mov16_memaddr' );
	
	push @asm, "mov AL, [qword 0xFF00FF00FF00FF00]";
	push @asm, "mov [qword 0xFF00FF00FF00FF00], AL";
	push @out, new_writer->mov8_al_memaddr(0xFF00FF00FF00FF00)->bytes;
	push @out, new_writer->mov8_memaddr_al(0xFF00FF00FF00FF00)->bytes;
	asm_ok( \@out, \@asm, 'mov8_memaddr' );
}

sub test_mov_mem_imm {
	my (@asm, @out);
	iterate_mem_addr_combos(
		\@asm, sub { "mov byte $_[0], 42" },
		\@out, sub { new_writer->mov8_mem_imm([@_], 42)->bytes },
	);
	asm_ok( \@out, \@asm, 'mov8_mem_imm' );
	iterate_mem_addr_combos(
		\@asm, sub { "mov word $_[0], 42" },
		\@out, sub { new_writer->mov16_mem_imm([@_], 42)->bytes },
	);
	asm_ok( \@out, \@asm, 'mov16_mem_imm' );
	iterate_mem_addr_combos(
		\@asm, sub { "mov dword $_[0], 42" },
		\@out, sub { new_writer->mov32_mem_imm([@_], 42)->bytes },
	);
	asm_ok( \@out, \@asm, 'mov32_mem_imm' );
	iterate_mem_addr_combos(
		\@asm, sub { "mov qword $_[0], 42" },
		\@out, sub { new_writer->mov64_mem_imm([@_], 42)->bytes },
	);
	asm_ok( \@out, \@asm, 'mov64_mem_imm' );
}

sub test_lea {
	my (@asm, @out);
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "lea $reg, $_[0]" },
			\@out, sub { new_writer->lea16_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'lea16_mem_reg' );
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "lea $reg, $_[0]" },
			\@out, sub { new_writer->lea32_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'lea632_mem_reg' );
	for my $reg (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "lea $reg, $_[0]" },
			\@out, sub { new_writer->lea64_reg_mem($reg, [@_])->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'lea64_mem_reg' );

	done_testing;
}


subtest mov_reg => \&test_mov_reg;
subtest mov_const => \&test_mov_const;
subtest mov_mem => \&test_mov_mem;
subtest mov_mem_imm => \&test_mov_mem_imm;
subtest mov_ax_addr => \&test_mov_ax_addr;
subtest lea => \&test_lea;
done_testing;
