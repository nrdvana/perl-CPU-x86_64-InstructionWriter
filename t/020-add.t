#! /usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use TestASM qw( new_writer iterate_mem_addr_combos asm_ok @r64 @r32 @r16 @r8 @immed32 @immed16 @immed8 );
use Test::More;
use Log::Any::Adapter 'TAP';

subtest add_reg => \&add_reg;
sub add_reg {
	my (@asm, @out);
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "add $r1, $r2";
			push @out, new_writer->add64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'add reg64, reg64' );
	
	@asm= (); @out= ();
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "add $r1, $r2";
			push @out, new_writer->add32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'add reg32, reg32' );
	
	@asm= (); @out= ();
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "add $r1, $r2";
			push @out, new_writer->add16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'add reg16, reg16' );
	
	@asm= (); @out= ();
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "add $r1, $r2";
			push @out, new_writer->add8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'add reg8, reg8' );
	
	done_testing;
}

subtest add_const => \&add_const;
sub add_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed32) {
			push @asm, "add $dst, $val";
			push @out, new_writer->add64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "add $dst, $val";
			push @out, new_writer->add32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "add $dst, $val";
			push @out, new_writer->add16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for my $val (@immed8) {
			push @asm, "add $dst, $val";
			push @out, new_writer->add8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'mov8_const' );
}

subtest add_mem => \&add_mem;
sub add_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "add qword $dst, $_[0]" },
			\@out, sub { new_writer->add64_mem($dst, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "add qword $_[0], $dst" },
			\@out, sub { new_writer->add64_to_mem($dst, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "add dword $reg, $_[0]" },
			\@out, sub { new_writer->add32_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "add dword $_[0], $reg" },
			\@out, sub { new_writer->add32_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "add word $reg, $_[0]" },
			\@out, sub { new_writer->add16_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "add word $_[0], $reg" },
			\@out, sub { new_writer->add16_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "add byte $reg, $_[0]" },
			\@out, sub { new_writer->add8_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "add byte $_[0], $reg" },
			\@out, sub { new_writer->add8_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add8_mem' );

	done_testing;
}

subtest add_const_mem => \&add_const_mem;
sub add_const_mem {
	my (@asm, @out);
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "add qword $_[0], $immed" },
			\@out, sub { new_writer->add64_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "add dword $_[0], $immed" },
			\@out, sub { new_writer->add32_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed16) {
		iterate_mem_addr_combos(
			\@asm, sub { "add word $_[0], $immed" },
			\@out, sub { new_writer->add16_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed8) {
		iterate_mem_addr_combos(
			\@asm, sub { "add byte $_[0], $immed" },
			\@out, sub { new_writer->add8_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'add8_const_to_mem' );
	
	done_testing;
}

subtest addcarry_reg => \&addcarry_reg;
sub addcarry_reg {
	my (@asm, @out);
	for my $r1 (@r64) {
		for my $r2 (@r64) {
			push @asm, "adc $r1, $r2";
			push @out, new_writer->addcarry64_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry64_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r32) {
		for my $r2 (@r32) {
			push @asm, "adc $r1, $r2";
			push @out, new_writer->addcarry32_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry32_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r16) {
		for my $r2 (@r16) {
			push @asm, "adc $r1, $r2";
			push @out, new_writer->addcarry16_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry16_reg' );
	
	@asm= (); @out= ();
	for my $r1 (@r8) {
		for my $r2 (@r8) {
			push @asm, "adc $r1, $r2";
			push @out, new_writer->addcarry8_reg($r1, $r2)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry8_reg' );
	
	done_testing;
}

subtest addcarry_const => \&addcarry_const;
sub addcarry_const {
	# Test immediate values of every bit length
	my (@asm, @out);
	for my $dst (@r64) {
		for my $val (@immed32) {
			push @asm, "adc $dst, $val";
			push @out, new_writer->addcarry64_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry64_const' );
	
	@asm= (); @out= ();
	for my $dst (@r32) {
		for my $val (@immed32) {
			push @asm, "adc $dst, $val";
			push @out, new_writer->addcarry32_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry32_const' );
	
	@asm= (); @out= ();
	for my $dst (@r16) {
		for my $val (@immed16) {
			push @asm, "adc $dst, $val";
			push @out, new_writer->addcarry16_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry16_const' );

	@asm= (); @out= ();
	for my $dst (@r8) {
		for my $val (@immed8) {
			push @asm, "adc $dst, $val";
			push @out, new_writer->addcarry8_const($dst, $val)->bytes;
		}
	}
	asm_ok( \@out, \@asm, 'addcarry8_const' );
}

subtest addcarry_mem => \&addcarry_mem;
sub addcarry_mem {
	my (@asm, @out);
	for my $dst (@r64) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc qword $dst, $_[0]" },
			\@out, sub { new_writer->addcarry64_mem($dst, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "adc qword $_[0], $dst" },
			\@out, sub { new_writer->addcarry64_to_mem($dst, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry64_mem' );

	@asm= (); @out= ();
	for my $reg (@r32) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc dword $reg, $_[0]" },
			\@out, sub { new_writer->addcarry32_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "adc dword $_[0], $reg" },
			\@out, sub { new_writer->addcarry32_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry32_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r16) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc word $reg, $_[0]" },
			\@out, sub { new_writer->addcarry16_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "adc word $_[0], $reg" },
			\@out, sub { new_writer->addcarry16_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry16_mem' );
	
	@asm= (); @out= ();
	for my $reg (@r8) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc byte $reg, $_[0]" },
			\@out, sub { new_writer->addcarry8_mem($reg, @_)->bytes }
		);
		iterate_mem_addr_combos(
			\@asm, sub { "adc byte $_[0], $reg" },
			\@out, sub { new_writer->addcarry8_to_mem($reg, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry8_mem' );

	done_testing;
}

subtest addcarry_const_mem => \&addcarry_const_mem;
sub addcarry_const_mem {
	my (@asm, @out);
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc qword $_[0], $immed" },
			\@out, sub { new_writer->addcarry64_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry64_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed32) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc dword $_[0], $immed" },
			\@out, sub { new_writer->addcarry32_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry32_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed16) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc word $_[0], $immed" },
			\@out, sub { new_writer->addcarry16_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry16_const_to_mem' );
	
	@asm= (); @out= ();
	for my $immed (@immed8) {
		iterate_mem_addr_combos(
			\@asm, sub { "adc byte $_[0], $immed" },
			\@out, sub { new_writer->addcarry8_const_to_mem($immed, @_)->bytes }
		);
	}
	asm_ok( \@out, \@asm, 'addcarry8_const_to_mem' );
	
	done_testing;
}

done_testing;
