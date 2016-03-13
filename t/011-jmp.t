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

subtest forward_short => \&forward_short;
sub forward_short {
	my (@asm, @out);
	my $label= 0;
	for my $op (qw( jmp je jne ja jae jb jbe jl jle jg jge js jns jo jno jpe jpo jrcxz loop loopz loopnz )) {
		++$label;
		push @asm, "$op label$label\nnop\nlabel$label: nop\n";
		push @out, new_writer->$op("label$label")->nop->mark("label$label")->nop->bytes;
	}
	asm_ok( \@out, \@asm, 'conditional jump' );
	done_testing;
}

subtest backward_short => \&backward_short;
sub backward_short {
	my (@asm, @out);
	my $label= 0;
	for my $op (qw( jmp je jne ja jae jb jbe jl jle jg jge js jns jo jno jpe jpo jrcxz loop loopz loopnz )) {
		++$label;
		push @asm, "label$label: nop\n$op label$label\n";
		push @out, new_writer->mark("label$label")->nop->$op("label$label")->bytes;
	}
	asm_ok( \@out, \@asm, 'conditional jump' );
	done_testing;
}

subtest jmp_abs_reg => \&jmp_abs_reg;
sub jmp_abs_reg {
	my (@asm, @out);
	for my $reg (@r64) {
		push @asm, "jmp $reg";
		push @out, new_writer->jmp_abs_reg($reg)->bytes;
	}
	asm_ok( \@out, \@asm, 'jump REG' );
	
	done_testing;
}

subtest jmp_abs_mem => \&jmp_abs_mem;
sub jmp_abs_mem {
	my (@asm, @out);
	for my $reg (@r64) {
		push @asm, "jmp [$reg]\n";
		push @out, new_writer->jmp_abs_mem($reg)->bytes;
	}
	asm_ok( \@out, \@asm, 'jmp [REG]' );
	
	@asm= (); @out= ();
	for my $r1 (@r64) {
		push @asm, "jmp [$r1+7Fh]\n" . "jmp [$r1-80h]\n";
		push @out, new_writer->jmp_abs_mem($r1, 0x7F)->jmp_abs_mem($r1, -0x80)->bytes;
	}
	asm_ok( \@out, \@asm, 'jmp [reg + disp]' );

	@asm= (); @out= ();
	for my $r2 (grep { $_ ne 'rsp' } @r64) {
		for my $r1 (@r64) {
			push @asm, "jmp [$r1 + $r2*4]\n"
				. "jmp [$r1+7Fh + $r2*2]\n"
				. "jmp [$r1-80h + $r2*8]\n";
			push @out, new_writer
				->jmp_abs_mem($r1, 0, $r2, 4)
				->jmp_abs_mem($r1, 0x7F, $r2, 2)
				->jmp_abs_mem($r1, -0x80, $r2, 8)
				->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'jmp [reg + disp + reg2 * scale' );
	
	done_testing;
}

subtest loop => \&loop;
sub loop {
	my (@asm, @out);
	my $loop= 0;
	
	for my $r1 (@r64) {
		push @asm, "jmp [$r1]\n";
		push @out, new_writer->jmp_abs_mem($r1)->bytes;
	}
	asm_ok( \@out, \@asm, 'jmp [reg]' );
	
	done_testing;
}

done_testing;