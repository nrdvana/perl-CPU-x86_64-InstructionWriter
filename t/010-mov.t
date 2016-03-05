#! /usr/bin/env perl
use strict;
use warnings;
use Log::Any::Adapter 'TAP';
use Test::More;
use IO::Handle;
use File::Temp;
use FindBin;
use lib "$FindBin::Bin/../perl_lib";
use CPU::x86_64::InstructionWriter;

sub new_writer { CPU::x86_64::InstructionWriter->new };

sub reference_assemble {
	# Can't pipe to nasm, because it tries to mmap the input, or something.
	# So use clumsy temp files.
	my $infile= File::Temp->new(TEMPLATE => 'asm-mov-XXXXXX', SUFFIX => '.asm')
		or die "tmpfile: $!";
	$infile->print("[bits 64]\n".$_[0]);
	$infile->flush;
	my $outfile= File::Temp->new(TEMPLATE => 'asm-mov-XXXXXX', SUFFIX => '.o')
		or die "tmpfile: $!";
	unless (system('nasm', '-o', $outfile, $infile) == 0) {
		my $e= $?;
		system('cp', $infile, 'died.asm');
		die "nasm: $e (input = died.asm)";
	}
	binmode $outfile;
	$outfile->seek(0,0);
	local $/= undef;
	scalar <$outfile>;
}

sub hex_diff {
	my ($data1, $data2)= @_;
	my $o= 0;
	my $ret= '';
	while ($o < length($data1) || $o < length($data2)) {
		my $d1= length $data1 >= $o? substr($data1, $o, 16) : '';
		my $d2= length $data2 >= $o? substr($data2, $o, 16) : '';
		$d1 =~ s/(.)/sprintf("%02x ",ord($1))/gse;
		substr($d1, 24, 0)= ' ' if length $d1 > 24;
		$d2 =~ s/(.)/sprintf("%02x ",ord($1))/gse;
		substr($d2, 24, 0)= ' ' if length $d2 > 24;
		$ret .= sprintf("%-48s |  %-48s%s\n", $d1, $d2, $d1 eq $d2? '':' *');
		$o+= 16;
	}
	return $ret;
}

sub test_mov_reg_reg {
	# Generate every combination of to/from registers
	my $asm= '';
	my $assembler= new_writer;
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	for my $src (@r64) {
		for my $dst (@r64) {
			$asm   .= "mov $dst, $src;\n";
			$assembler->mov64_from_reg($dst, $src);
		}
	}
	my $bytes= $assembler->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '64-bit reg-to-reg instructions' )
		or diag(hex_diff($bytes, $ref_bytes));
	done_testing;
}

sub test_mov_reg_imm {
	# Test immediate values of every bit length
	my $asm= '';
	my $assembler= new_writer;
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	for my $dst (@r64) {
		for (my $bits= 0; $bits < 64; $bits++) {
			$asm .= "mov $dst, ".(1 << $bits).";\n";
			$assembler->mov64_imm($dst, 1 << $bits);
		}
		# Also try special-case numbers
		no warnings 'portable';
		for my $val (qw( FFFF 7FFF FFFFFFFF 7FFFFFFF FFFFFFFFFFFFFFFF 7FFFFFFFFFFFFFFF )) {
			$asm .= "mov $dst, 0x$val;\n";
			$assembler->mov64_imm($dst, hex $val);
		}
	}
	my $bytes= $assembler->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '64-bit imm-to-reg instructions' )
		or diag(hex_diff($bytes, $ref_bytes));
	done_testing;
}

sub test_mov_load_reg {
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	my $asm= '';
	my $assembler= new_writer;
	for my $dst (@r64) {
		for my $src (@r64) {
			$asm .= "mov $dst, [$src];\n";
			$assembler->mov64_from_mem($dst, $src);
			for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
				$asm .= "mov $dst, [$src+$ofs];\n";
				$assembler->mov64_from_mem($dst, $src, $ofs, undef, undef);
			}
		}
	}
	my $bytes= $assembler->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '64-bit load-reg-addr instructions' )
		or diag(hex_diff($bytes, $ref_bytes));
	done_testing;
}

sub test_mov_load_reg_index {
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	my $asm;
	my $assembler= new_writer;
	for my $dst (@r64) {
		$asm= '';
		$assembler= $assembler->new;
		for my $src (@r64) {
			# RSP isn't valid as a multiplier register
			for my $rofs (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					$asm .= "mov $dst, [$src+$rofs*$mul];\n";
					$assembler->mov64_from_mem($dst, $src, undef, $rofs, $mul);
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						$asm .= "mov $dst, [$src+$ofs+$rofs*$mul];\n";
						$assembler->mov64_from_mem($dst, $src, $ofs, $rofs, $mul);
					}
				}
			}
		}
		my $bytes= $assembler->bytes;
		my $ref_bytes= reference_assemble($asm);
		is( $bytes, $ref_bytes, "64-bit load-reg-addr instructions $dst := ..." )
			or diag(hex_diff($bytes, $ref_bytes)), die;
	}
	done_testing;
}

sub test_mov_stor_reg_index {
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	my $asm;
	my $assembler= new_writer;
	for my $dst (@r64) {
		$asm= '';
		$assembler= $assembler->new;
		for my $src (@r64) {
			# RSP isn't valid as a multiplier register
			for my $rofs (grep { $_ ne 'rsp' } @r64) {
				for my $mul (1, 2, 4, 8) {
					$asm .= "mov [$dst+$rofs*$mul], $src;\n";
					$assembler->mov64_to_mem($src, $dst, undef, $rofs, $mul);
					for my $ofs (0, 1, -1, 0x7, -0x8, 0x7F, -0x80, 0x7FFF, -0x8000, 0x7FFFFFFF, -0x80000000) {
						$asm .= "mov [$dst+$ofs+$rofs*$mul], $src;\n";
						$assembler->mov64_to_mem($src, $dst, $ofs, $rofs, $mul);
					}
				}
			}
		}
		my $bytes= $assembler->bytes;
		my $ref_bytes= reference_assemble($asm);
		is( $bytes, $ref_bytes, "64-bit load-reg-addr instructions $dst := ..." )
			or diag(hex_diff($bytes, $ref_bytes)), die;
	}
	done_testing;
}

subtest mov_reg_reg => \&test_mov_reg_reg;
subtest mov_reg_imm => \&test_mov_reg_imm;
subtest mov_load_reg => \&test_mov_load_reg;
subtest mov_load_reg_index => \&test_mov_load_reg_index;
subtest mov_stor_reg_index => \&test_mov_stor_reg_index;
done_testing;
