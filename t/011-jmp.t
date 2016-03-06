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

subtest forward_short => \&forward_short;
sub forward_short {
	my $asm= '';
	my $writer= new_writer;
	my $label= 0;
	for my $op (qw( jmp je jne ja jae jb jbe jl jle jg jge js jns jo jno jpe jpo jrcxz loop loopz loopnz )) {
		++$label;
		$asm .= "$op label$label\nnop\nlabel$label: nop\n";
		$writer->$op("label$label")->nop->mark("label$label")->nop;
	}
	my $bytes= $writer->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '64-bit reg-to-reg instructions' )
		or diag(hex_diff($bytes, $ref_bytes));
	done_testing;
}

subtest backward_short => \&backward_short;
sub backward_short {
	my $asm= '';
	my $writer= new_writer;
	my $label= 0;
	for my $op (qw( jmp je jne ja jae jb jbe jl jle jg jge js jns jo jno jpe jpo jrcxz loop loopz loopnz )) {
		++$label;
		$asm .= "label$label: nop\n$op label$label\n";
		$writer->mark("label$label")->nop->$op("label$label");
	}
	my $bytes= $writer->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '64-bit reg-to-reg instructions' )
		or diag(hex_diff($bytes, $ref_bytes));
	done_testing;
}

subtest jmp_abs_reg => \&jmp_abs_reg;
sub jmp_abs_reg {
	my $asm= '';
	my $writer= new_writer;
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	for my $reg (@r64) {
		$asm .= "jmp $reg\n";
		$writer->jmp_abs_reg($reg);
	}
	my $bytes= $writer->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes )
		or diag(hex_diff($bytes, $ref_bytes));
	
	done_testing;
}

subtest jmp_abs_mem => \&jmp_abs_mem;
sub jmp_abs_mem {
	my $asm= '';
	my $writer= new_writer;
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	for my $r1 (@r64) {
		$asm .= "jmp [$r1]\n";
		$writer->jmp_abs_mem($r1);
	}
	my $bytes= $writer->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '[reg]' )
		or diag(hex_diff($bytes, $ref_bytes));
	
	$asm= '';
	$writer= new_writer;
	for my $r1 (@r64) {
		$asm .= "jmp [$r1+7Fh]\n" . "jmp [$r1-80h]\n";
		$writer->jmp_abs_mem($r1, 0x7F)->jmp_abs_mem($r1, -0x80);
	}
	$bytes= $writer->bytes;
	$ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '[reg + disp]' )
		or diag(hex_diff($bytes, $ref_bytes));
	
	for my $r2 (qw( rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 )) {
		$asm= '';
		$writer= new_writer;
		for my $r1 (@r64) {
			$asm .= "jmp [$r1 + $r2*4]\n"
				. "jmp [$r1+7Fh + $r2*2]\n"
				. "jmp [$r1-80h + $r2*8]\n";
			$writer->jmp_abs_mem($r1, 0, $r2, 4)
				->jmp_abs_mem($r1, 0x7F, $r2, 2)
				->jmp_abs_mem($r1, -0x80, $r2, 8);
		}
		$bytes= $writer->bytes;
		$ref_bytes= reference_assemble($asm);
		is( $bytes, $ref_bytes, "[reg + disp + $r2 * scale]" )
			or diag(hex_diff($bytes, $ref_bytes));
	}
	
	done_testing;
}

subtest loop => \&loop;
sub loop {
	my $asm= '';
	my $writer= new_writer;
	my $loop= 0;
	
	my @r64= qw( rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15 );
	for my $r1 (@r64) {
		$asm .= "jmp [$r1]\n";
		$writer->jmp_abs_mem($r1);
	}
	my $bytes= $writer->bytes;
	my $ref_bytes= reference_assemble($asm);
	is( $bytes, $ref_bytes, '[reg]' )
		or diag(hex_diff($bytes, $ref_bytes));
	
	done_testing;
}

done_testing;