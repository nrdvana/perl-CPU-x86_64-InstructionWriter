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

subtest not => \&not;
sub not {
	my (@asm, @out);
	
	for my $r1 (@r64) {
		push @asm, "not $r1";
		push @out, new_writer->not64_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "not qword [$rbase]";
		push @out, new_writer->not64_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "not qword [$rbase+$ofs]";
			push @out, new_writer->not64_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "not qword [$rbase+$ridx*$mul]";
				push @out, new_writer->not64_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "not qword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->not64_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'not64');

	for my $r1 (@r32) {
		push @asm, "not $r1";
		push @out, new_writer->not32_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "not dword [$rbase]";
		push @out, new_writer->not32_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "not dword [$rbase+$ofs]";
			push @out, new_writer->not32_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "not dword [$rbase+$ridx*$mul]";
				push @out, new_writer->not32_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "not dword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->not32_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'not32');

	for my $r1 (@r16) {
		push @asm, "not $r1";
		push @out, new_writer->not16_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "not word [$rbase]";
		push @out, new_writer->not16_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "not word [$rbase+$ofs]";
			push @out, new_writer->not16_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "not word [$rbase+$ridx*$mul]";
				push @out, new_writer->not16_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "not word [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->not16_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'not16');
	
	for my $r1 (@r8) {
		push @asm, "not $r1";
		push @out, new_writer->not8_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "not byte [$rbase]";
		push @out, new_writer->not8_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "not byte [$rbase+$ofs]";
			push @out, new_writer->not8_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "not byte [$rbase+$ridx*$mul]";
				push @out, new_writer->not8_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "not byte [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->not8_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'not8');

	done_testing;
}

subtest neg => \&neg;
sub neg {
	my (@asm, @out);
	
	for my $r1 (@r64) {
		push @asm, "neg $r1";
		push @out, new_writer->neg64_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "neg qword [$rbase]";
		push @out, new_writer->neg64_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "neg qword [$rbase+$ofs]";
			push @out, new_writer->neg64_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "neg qword [$rbase+$ridx*$mul]";
				push @out, new_writer->neg64_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "neg qword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->neg64_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'neg64');

	for my $r1 (@r32) {
		push @asm, "neg $r1";
		push @out, new_writer->neg32_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "neg dword [$rbase]";
		push @out, new_writer->neg32_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "neg dword [$rbase+$ofs]";
			push @out, new_writer->neg32_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "neg dword [$rbase+$ridx*$mul]";
				push @out, new_writer->neg32_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "neg dword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->neg32_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'neg32');

	for my $r1 (@r16) {
		push @asm, "neg $r1";
		push @out, new_writer->neg16_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "neg word [$rbase]";
		push @out, new_writer->neg16_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "neg word [$rbase+$ofs]";
			push @out, new_writer->neg16_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "neg word [$rbase+$ridx*$mul]";
				push @out, new_writer->neg16_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "neg word [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->neg16_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'neg16');
	
	for my $r1 (@r8) {
		push @asm, "neg $r1";
		push @out, new_writer->neg8_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "neg byte [$rbase]";
		push @out, new_writer->neg8_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "neg byte [$rbase+$ofs]";
			push @out, new_writer->neg8_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "neg byte [$rbase+$ridx*$mul]";
				push @out, new_writer->neg8_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "neg byte [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->neg8_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'neg8');

	done_testing;
}

done_testing;