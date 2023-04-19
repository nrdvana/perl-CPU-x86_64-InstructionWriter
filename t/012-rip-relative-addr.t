#! /usr/bin/env perl
use strict;
use warnings;
no warnings 'portable';
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @r8h @immed64 @immed32 @immed16 @immed8 unknown );
use Test::More;
use Log::Any::Adapter 'TAP';

sub test_mov_mem_rel_ofs {
	my (@asm, @out);
	my $w= new_writer;
	$w->start_address(0x2000);
	push @asm, (("nop") x 0x10), 'mov rip[0xFF0], rax';
	push @out, $w->nop(0x10)->mov64_mem_reg([ 'rip', $w->offset_to(0x3000) ], 'rax')->bytes;
	asm_ok( \@out, \@asm, 'mov64_mem_*' );

	done_testing;
}

subtest mov_mem_rel_ofs => \&test_mov_mem_rel_ofs;
done_testing;
