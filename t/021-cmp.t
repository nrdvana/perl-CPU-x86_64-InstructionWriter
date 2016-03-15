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

subtest cmp_reg => \&cmp_reg;
sub cmp_reg {
	my (@asm, @out);
	my $loop= 0;
	
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg64, reg64' );
	
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg32, reg32' );
	
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg16, reg16' );
	
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "cmp $r1, $r2";
			push @out, new_writer->cmp8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'cmp reg8, reg8' );
	
	done_testing;
}

subtest cmp_const => \&cmp_const;
sub cmp_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for (my $bits= 0; $bits < 31; $bits++) {
			push @asm, "cmp $dst, ".(1 << $bits);
			push @out, new_writer->cmp64_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		no warnings 'portable';
		for my $val (0xFF, 0x7F, 0xFFFF, 0x7FFF, 0x7FFFFFFF, -1) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for (my $bits= 0; $bits < 31; $bits++) {
			push @asm, "cmp $dst, ".(1 << $bits);
			push @out, new_writer->cmp32_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		no warnings 'portable';
		for my $val (0xFF, 0x7F, 0xFFFF, 0x7FFF, 0x7FFFFFFF, 0xFFFFFFFF, -1) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for (my $bits= 0; $bits < 15; $bits++) {
			push @asm, "cmp $dst, ".(1 << $bits);
			push @out, new_writer->cmp16_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		no warnings 'portable';
		for my $val (0xFF, 0x7F, 0x7FFF, 0xFFFF, -1) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for (my $bits= 0; $bits < 7; $bits++) {
			push @asm, "cmp $dst, ".(1 << $bits);
			push @out, new_writer->cmp8_const($dst, 1 << $bits)->bytes;
		}
		# Also try special-case numbers
		no warnings 'portable';
		for my $val (0x7F, -1, 0xFF) {
			push @asm, "cmp $dst, $val";
			push @out, new_writer->cmp8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );
}

subtest cmp_mem => \&cmp_mem;
sub cmp_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		for my $src (@r64) {
			push @asm, "cmp $dst, [$src]";
			push @out, new_writer->cmp64_mem($dst, $src)->bytes;
			push @asm, "cmp [$dst], $src";
			push @out, new_writer->cmp64_to_mem($src, $dst)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp $dst, [$src+$ofs]";
				push @out, new_writer->cmp64_mem($dst, $src, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					push @asm, "cmp $dst, [$src+$ridx*$mul]";
					push @out, new_writer->cmp64_mem($dst, $src, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp $dst, [$src+$ofs+$ridx*$mul]";
						push @out, new_writer->cmp64_mem($dst, $src, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		for my $rbase (@r64) {
			push @asm, "cmp $reg, [$rbase]";
			push @out, new_writer->cmp32_mem($reg, $rbase)->bytes;
			push @asm, "cmp [$rbase], $reg";
			push @out, new_writer->cmp32_to_mem($reg, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp $reg, [$rbase+$ofs]";
				push @out, new_writer->cmp32_mem($reg, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					push @asm, "cmp $reg, [$rbase+$ridx*$mul]";
					push @out, new_writer->cmp32_mem($reg, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp $reg, [$rbase+$ofs+$ridx*$mul]";
						push @out, new_writer->cmp32_mem($reg, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		for my $rbase (@r64) {
			push @asm, "cmp $reg, [$rbase]";
			push @out, new_writer->cmp16_mem($reg, $rbase)->bytes;
			push @asm, "cmp [$rbase], $reg";
			push @out, new_writer->cmp16_to_mem($reg, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp $reg, [$rbase+$ofs]";
				push @out, new_writer->cmp16_mem($reg, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					push @asm, "cmp $reg, [$rbase+$ridx*$mul]";
					push @out, new_writer->cmp16_mem($reg, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp $reg, [$rbase+$ofs+$ridx*$mul]";
						push @out, new_writer->cmp16_mem($reg, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		for my $rbase (@r64) {
			push @asm, "cmp $reg, [$rbase]";
			push @out, new_writer->cmp8_mem($reg, $rbase)->bytes;
			push @asm, "cmp [$rbase], $reg";
			push @out, new_writer->cmp8_to_mem($reg, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp $reg, [$rbase+$ofs]";
				push @out, new_writer->cmp8_mem($reg, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					push @asm, "cmp $reg, [$rbase+$ridx*$mul]";
					push @out, new_writer->cmp8_mem($reg, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp $reg, [$rbase+$ofs+$ridx*$mul]";
						push @out, new_writer->cmp8_mem($reg, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp8_mem' );

	done_testing;
}

subtest cmp_const_mem => \&cmp_const_mem;
sub cmp_const_mem {
	my (@asm, @out);
	for my $immed (0xFF, 0x7F, 0xFFFF, 0x7FFF, 0x7FFFFFFF, -1, (map { 1<<$_ } 0..30)) {
		for my $rbase (@r64) {
			push @asm, "cmp qword [$rbase], $immed";
			push @out, new_writer->cmp64_const_to_mem($immed, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp qword [$rbase+$ofs], $immed";
				push @out, new_writer->cmp64_const_to_mem($immed, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (4) {
					push @asm, "cmp qword [$rbase+$ridx*$mul], $immed";
					push @out, new_writer->cmp64_const_to_mem($immed, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp qword [$rbase+$ofs+$ridx*$mul], $immed";
						push @out, new_writer->cmp64_const_to_mem($immed, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (0xFF, 0x7F, 0xFFFF, 0x7FFF, 0x7FFFFFFF, -1, (map { 1<<$_ } 0..31)) {
		for my $rbase (@r64) {
			push @asm, "cmp dword [$rbase], $immed";
			push @out, new_writer->cmp32_const_to_mem($immed, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp dword [$rbase+$ofs], $immed";
				push @out, new_writer->cmp32_const_to_mem($immed, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (4) {
					push @asm, "cmp dword [$rbase+$ridx*$mul], $immed";
					push @out, new_writer->cmp32_const_to_mem($immed, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp dword [$rbase+$ofs+$ridx*$mul], $immed";
						push @out, new_writer->cmp32_const_to_mem($immed, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (0xFF, 0x7F, 0xFFFF, 0x7FFF, -1, (map { 1<<$_ } 0..15)) {
		for my $rbase (@r64) {
			push @asm, "cmp word [$rbase], $immed";
			push @out, new_writer->cmp16_const_to_mem($immed, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp word [$rbase+$ofs], $immed";
				push @out, new_writer->cmp16_const_to_mem($immed, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (4) {
					push @asm, "cmp word [$rbase+$ridx*$mul], $immed";
					push @out, new_writer->cmp16_const_to_mem($immed, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp word [$rbase+$ofs+$ridx*$mul], $immed";
						push @out, new_writer->cmp16_const_to_mem($immed, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (0xFF, 0x7F, -1, (map { 1<<$_ } 0..7)) {
		for my $rbase (@r64) {
			push @asm, "cmp byte [$rbase], $immed";
			push @out, new_writer->cmp8_const_to_mem($immed, $rbase)->bytes;
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				push @asm, "cmp byte [$rbase+$ofs], $immed";
				push @out, new_writer->cmp8_const_to_mem($immed, $rbase, $ofs)->bytes;
			}
			# RSP isn't valid as a multiplier register
			for my $ridx (grep { $_ ne 'rsp' } @r64) {
				for my $mul (4) {
					push @asm, "cmp byte [$rbase+$ridx*$mul], $immed";
					push @out, new_writer->cmp8_const_to_mem($immed, $rbase, undef, $ridx, $mul)->bytes;
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						push @asm, "cmp byte [$rbase+$ofs+$ridx*$mul], $immed";
						push @out, new_writer->cmp8_const_to_mem($immed, $rbase, $ofs, $ridx, $mul)->bytes;
					}
				}
			}
		}
	}
	asm_ok( \@out, \@asm, 'cmp8_const_to_mem' );
	
	done_testing;
}

done_testing;