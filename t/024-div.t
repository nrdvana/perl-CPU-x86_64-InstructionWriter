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
my @r8 = qw( al cl dl bl spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b );

	my (@asm, @out);
	
	for my $r1 (@r64) {
		push @asm, "div $r1";
		push @out, new_writer->div64u_reg($r1)->bytes;
		push @asm, "idiv $r1";
		push @out, new_writer->div64s_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "div qword [$rbase]";
		push @out, new_writer->div64u_mem($rbase)->bytes;
		push @asm, "idiv qword [$rbase]";
		push @out, new_writer->div64s_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "div qword [$rbase+$ofs]";
			push @out, new_writer->div64u_mem($rbase, $ofs)->bytes;
			push @asm, "idiv qword [$rbase+$ofs]";
			push @out, new_writer->div64s_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "div qword [$rbase+$ridx*$mul]";
				push @out, new_writer->div64u_mem($rbase, undef, $ridx, $mul)->bytes;
				push @asm, "idiv qword [$rbase+$ridx*$mul]";
				push @out, new_writer->div64s_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "div qword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div64u_mem($rbase, $ofs, $ridx, $mul)->bytes;
					push @asm, "idiv qword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div64s_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'div64');

	for my $r1 (@r32) {
		push @asm, "div $r1";
		push @out, new_writer->div32u_reg($r1)->bytes;
		push @asm, "idiv $r1";
		push @out, new_writer->div32s_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "div dword [$rbase]";
		push @out, new_writer->div32u_mem($rbase)->bytes;
		push @asm, "idiv dword [$rbase]";
		push @out, new_writer->div32s_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "div dword [$rbase+$ofs]";
			push @out, new_writer->div32u_mem($rbase, $ofs)->bytes;
			push @asm, "idiv dword [$rbase+$ofs]";
			push @out, new_writer->div32s_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "div dword [$rbase+$ridx*$mul]";
				push @out, new_writer->div32u_mem($rbase, undef, $ridx, $mul)->bytes;
				push @asm, "idiv dword [$rbase+$ridx*$mul]";
				push @out, new_writer->div32s_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "div dword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div32u_mem($rbase, $ofs, $ridx, $mul)->bytes;
					push @asm, "idiv dword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div32s_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'div32');

	for my $r1 (@r16) {
		push @asm, "div $r1";
		push @out, new_writer->div16u_reg($r1)->bytes;
		push @asm, "idiv $r1";
		push @out, new_writer->div16s_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "div word [$rbase]";
		push @out, new_writer->div16u_mem($rbase)->bytes;
		push @asm, "idiv word [$rbase]";
		push @out, new_writer->div16s_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "div word [$rbase+$ofs]";
			push @out, new_writer->div16u_mem($rbase, $ofs)->bytes;
			push @asm, "idiv word [$rbase+$ofs]";
			push @out, new_writer->div16s_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "div word [$rbase+$ridx*$mul]";
				push @out, new_writer->div16u_mem($rbase, undef, $ridx, $mul)->bytes;
				push @asm, "idiv word [$rbase+$ridx*$mul]";
				push @out, new_writer->div16s_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "div word [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div16u_mem($rbase, $ofs, $ridx, $mul)->bytes;
					push @asm, "idiv word [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div16s_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'div16');

	for my $r1 (@r8) {
		push @asm, "div $r1";
		push @out, new_writer->div8u_reg($r1)->bytes;
		push @asm, "idiv $r1";
		push @out, new_writer->div8s_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "div byte [$rbase]";
		push @out, new_writer->div8u_mem($rbase)->bytes;
		push @asm, "idiv byte [$rbase]";
		push @out, new_writer->div8s_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "div byte [$rbase+$ofs]";
			push @out, new_writer->div8u_mem($rbase, $ofs)->bytes;
			push @asm, "idiv byte [$rbase+$ofs]";
			push @out, new_writer->div8s_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "div byte [$rbase+$ridx*$mul]";
				push @out, new_writer->div8u_mem($rbase, undef, $ridx, $mul)->bytes;
				push @asm, "idiv byte [$rbase+$ridx*$mul]";
				push @out, new_writer->div8s_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "div byte [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div8u_mem($rbase, $ofs, $ridx, $mul)->bytes;
					push @asm, "idiv byte [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->div8s_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'div8');

done_testing;