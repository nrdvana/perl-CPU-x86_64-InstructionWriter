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

subtest inc => \&inc;
sub inc {
	my (@asm, @out);
	
	for my $r1 (@r64) {
		push @asm, "inc $r1";
		push @out, new_writer->inc64_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "inc qword [$rbase]";
		push @out, new_writer->inc64_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "inc qword [$rbase+$ofs]";
			push @out, new_writer->inc64_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "inc qword [$rbase+$ridx*$mul]";
				push @out, new_writer->inc64_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "inc qword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->inc64_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'inc64');

	for my $r1 (@r32) {
		push @asm, "inc $r1";
		push @out, new_writer->inc32_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "inc dword [$rbase]";
		push @out, new_writer->inc32_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "inc dword [$rbase+$ofs]";
			push @out, new_writer->inc32_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "inc dword [$rbase+$ridx*$mul]";
				push @out, new_writer->inc32_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "inc dword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->inc32_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'inc32');

	for my $r1 (@r16) {
		push @asm, "inc $r1";
		push @out, new_writer->inc16_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "inc word [$rbase]";
		push @out, new_writer->inc16_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "inc word [$rbase+$ofs]";
			push @out, new_writer->inc16_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "inc word [$rbase+$ridx*$mul]";
				push @out, new_writer->inc16_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "inc word [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->inc16_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'inc16');
	
	for my $r1 (@r8) {
		push @asm, "inc $r1";
		push @out, new_writer->inc8_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "inc byte [$rbase]";
		push @out, new_writer->inc8_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "inc byte [$rbase+$ofs]";
			push @out, new_writer->inc8_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "inc byte [$rbase+$ridx*$mul]";
				push @out, new_writer->inc8_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "inc byte [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->inc8_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'inc8');

	done_testing;
}

subtest dec => \&dec;
sub dec {
	my (@asm, @out);
	
	for my $r1 (@r64) {
		push @asm, "dec $r1";
		push @out, new_writer->dec64_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "dec qword [$rbase]";
		push @out, new_writer->dec64_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "dec qword [$rbase+$ofs]";
			push @out, new_writer->dec64_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "dec qword [$rbase+$ridx*$mul]";
				push @out, new_writer->dec64_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "dec qword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->dec64_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'dec64');

	for my $r1 (@r32) {
		push @asm, "dec $r1";
		push @out, new_writer->dec32_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "dec dword [$rbase]";
		push @out, new_writer->dec32_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "dec dword [$rbase+$ofs]";
			push @out, new_writer->dec32_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "dec dword [$rbase+$ridx*$mul]";
				push @out, new_writer->dec32_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "dec dword [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->dec32_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'dec32');

	for my $r1 (@r16) {
		push @asm, "dec $r1";
		push @out, new_writer->dec16_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "dec word [$rbase]";
		push @out, new_writer->dec16_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "dec word [$rbase+$ofs]";
			push @out, new_writer->dec16_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "dec word [$rbase+$ridx*$mul]";
				push @out, new_writer->dec16_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "dec word [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->dec16_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'dec16');
	
	for my $r1 (@r8) {
		push @asm, "dec $r1";
		push @out, new_writer->dec8_reg($r1)->bytes;
	}
	for my $rbase (@r64) {
		push @asm, "dec byte [$rbase]";
		push @out, new_writer->dec8_mem($rbase)->bytes;
		for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
			push @asm, "dec byte [$rbase+$ofs]";
			push @out, new_writer->dec8_mem($rbase, $ofs)->bytes;
		}
		# RSP isn't valid as a multiplier register
		for my $ridx (grep { $_ ne 'rsp' } @r64) {
			for my $mul (4) {
				push @asm, "dec byte [$rbase+$ridx*$mul]";
				push @out, new_writer->dec8_mem($rbase, undef, $ridx, $mul)->bytes;
				for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
					push @asm, "dec byte [$rbase+$ofs+$ridx*$mul]";
					push @out, new_writer->dec8_mem($rbase, $ofs, $ridx, $mul)->bytes;
				}
			}
		}
	}
	asm_ok(\@out, \@asm, 'dec8');

	done_testing;
}

done_testing;