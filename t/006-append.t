#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer asm_ok @r64 );
use Test::More;

my $inner= new_writer->cmp('rax', 'rcx')->jne('.done')->add('rax', 'rdx')->label('.done');
my $outer= new_writer->cmp('rax', 0)->jne('.done')->append($inner, '.inner')->push('rax')->label('.done');

my $asm= <<__;
	cmp rax, 0
	jne .done
	cmp rax, rcx
	jne .done2
	add rax, rdx
.done2:
	push rax
.done:
__

asm_ok([$outer->bytes], [$asm], 'nested jump to .done');

done_testing;
