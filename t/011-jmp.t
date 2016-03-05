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
	for my $op (qw( jmp je jne ja jae jb jbe jl jle jg jge js jns jo jno jpe jpo )) {
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
	for my $op (qw( jmp je jne ja jae jb jbe jl jle jg jge js jns jo jno jpe jpo )) {
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

done_testing;