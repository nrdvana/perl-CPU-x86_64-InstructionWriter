package CPU::x86_64::InstructionWriter;
use Moo;
use Carp;
use Exporter 'import';
use CPU::x86_64::InstructionWriter::Unknown;

=head1 SYNOPSIS

  # POSIX::exit(42);
  my $machine_code= CPU::x86_64::InstructionWriter->new
    ->mov64_imm( 'RAX', 60 )
    ->mov64_imm( 'RDI', 42 )
    ->syscall()
    ->bytes;

  # if (x == 1) { ++x } else { ++y }
  my ($else, $end);
  my $machine_code= CPU::x86_64::InstructionWriter->new
    ->cmp64_reg_imm( 'RAX', 0 )
    ->jne($else)        # jump to not-yet-defined label
    ->inc( 'RAX' )
    ->jmp($end)         # jump to another not-yet-defined label
    ->mark($else)       # resolve previous jump to this address
    ->inc( 'RCX' )
    ->mark($end)        # resolve second jump to this address
    ->bytes;

=head1 DESCRIPTION

The purpose of this module is to relatively efficiently assemble instructions for the x86_64
without generating and re-parsing assembly language, or shelling out to an external tool.
All instructions are assumed to be for the 64-bit mode of the processor.  Functionality for
real mode or segmented 16-bit mode will be handled by the yet-to-be-written x86 module.

This module consists of a bunch of chainable methods which build a string of machine code as
you call them.  It supports lazy-resolved jump labels, and lazy-bound constants which can be
assigned a value after the instructions have been assembled.

=cut

(0x7FFFFFFE << 31) > 0 && (0x7FFFFFFE << 63) == 0
	or die "Author is lazy and requires 64-bit perl integers\n";
no warnings 'portable';


my @byte_registers= qw( AH AL BH BL CH CL DH DL SPL BPL SIL DIL R8B R9B R10B R11B R12B R13B R14B R15B );
my %byte_register_alias= ( map {; "R${_}L" => "R${_}B" } 8..15 );
my @word_registers= qw( AX BX CX DX SI DI SP BP R8W R9W R10W R11W R12W R13W R14W R15W );
my @long_registers= qw( EAX EBX ECX EDX ESI EDI ESP EBP R8D R9D R10D R11D R12D R13D R14D R15D );
my @quad_registers= qw( RAX RBX RCX RDX RSI RDI RSP RBP R8 R9 R10 R11 R12 R13 R14 R15 RIP RFLAGS );
my @registers= ( @byte_registers, @word_registers, @long_registers, @quad_registers );
{
	# Create a constant for each register name
	no strict 'refs';
	eval 'sub '.$_.' { \''.$_.'\' } 1' || croak $@
		for @registers;
	*{__PACKAGE__."::$_"}= *{__PACKAGE__."::$byte_register_alias{$_}"}
		for keys %byte_register_alias;
}

sub unknown   { CPU::x86_64::InstructionWriter::Unknown->new(name => $_[0]); }
sub unknown8  { CPU::x86_64::InstructionWriter::Unknown->new(bits =>  8, name => $_[0]); }
sub unknown16 { CPU::x86_64::InstructionWriter::Unknown->new(bits => 16, name => $_[0]); }
sub unknown32 { CPU::x86_64::InstructionWriter::Unknown->new(bits => 32, name => $_[0]); }
sub unknown64 { CPU::x86_64::InstructionWriter::Unknown->new(bits => 64, name => $_[0]); }
sub unknown7  { CPU::x86_64::InstructionWriter::Unknown->new(bits =>  7, name => $_[0]); }
sub unknown15 { CPU::x86_64::InstructionWriter::Unknown->new(bits => 15, name => $_[0]); }
sub unknown31 { CPU::x86_64::InstructionWriter::Unknown->new(bits => 31, name => $_[0]); }
sub unknown63 { CPU::x86_64::InstructionWriter::Unknown->new(bits => 63, name => $_[0]); }

our %EXPORT_TAGS= (
	registers    => \@registers,
	unknown      => [qw( unknown unknown8 unknown16 unknown32 unknown64 unknown7 unknown15 unknown31 unknown63 )],
);
our @EXPORT_OK= ( map { @{$_} } values %EXPORT_TAGS );

has start_address         => ( is => 'rw', default => sub { unknown64() } );

has _buf                  => ( is => 'rw', default => sub { '' } );
has _unresolved           => ( is => 'rw', default => sub { [] } );
has labels                => ( is => 'rw', default => sub {; {} } );

sub get_label {
	my ($self, $name)= @_;
	my $labels= $self->labels;
	$name //= '__auto_'.(scalar keys %$labels);
	$labels->{$name} //= { name => $name };
}

=head2 mark

Bind a named label to the current position in the instruction buffer.  This position will shift
automatically if it follows instructions whose length is not yet known.  (as a consequence, you
should not use the numeric offset of the label until the instruction stream has been "resolved")

=cut

sub mark {
	@_ == 2 or croak "Invalid arguments to 'mark'";
	
	# If they gave an undefined label, we auto-populate it, which modifies
	# the variable they passed to this function.
	$_[1]= $_[0]->get_label
		unless defined $_[1];
	
	my ($self, $label)= @_;
	# If they give a label by name, auto-inflate it
	$label= $self->get_label($label)
		unless ref $label;
	
	# A label can only exist once
	defined $label->{start} and croak "Can't mark label '$label->{name}' twice";
	
	# Set the label's current location
	$label->{start}= length($self->{_buf});
	$label->{len}= 0;
	
	# Add it to the list of unresolved things, so its position can be updated
	push @{ $self->_unresolved }, $label;
	return $self;
}

=head2 bytes

Return the assembled instructions as a string of bytes.  This will fail if any of the labels were
left un-marked or if unknown placeholders have not been resolved.

=cut

sub bytes {
	my $self= shift;
	$self->_resolve;
	return $self->_buf;
}

=head1 INSTRUCTIONS

The following methods append an instruction to the buffer, and return C<$self> so you can continue
calling instructions in a chain.

=head2 nop

Insert a no-op byte.  TODO: add second argument that inserts nops until a specified alignment or
memory address.

=cut

sub nop {
	$_[0]{_buf} .= "\x90";
	$_[0];
}

# reg, reg
sub mov64_from_reg {
	shift->_append_reg_reg(0x08, 0x89, $_[1], $_[0]);
}

# reg, base, disp, index, mult
sub mov64_to_mem {
	shift->_append_reg_mem(0x08, 0x89, @_);
}

# reg, base, disp, index, mult
sub mov64_from_mem {
	shift->_append_reg_mem(0x08, 0x8B, @_);
}

# reg, immed
sub mov64_imm {
	my ($self, $reg, $immed)= @_;
	use integer;
	if (ref $immed) {
		my $value= $immed->value;
		unless (defined $value) {
			my $str;
			return $self->_mark_unresolved(
				10,
				calc_size => sub {
					$value= $immed->value
						// croak "$immed is not a defined value";
					$str= shift->_encode_mov64_imm($reg, $value);
					length $str;
				},
				encode => sub { $str },
			);
		}
		$immed= $value;
	}
	$self->{_buf} .= $self->_encode_mov64_imm($reg, $immed);
	$self;
}

=head2 JMP

All jump instructions are relative, and take either a numeric offset (from the start of the next
instruction) or a label, except the C<jmp_abs> instruction which takes a register containing the
target address, and the C<jmp_from_addr> which reads a memory address for the address to jump to.

=over

=item jmp

Unconditional jump to label (or 32-bit offset constant).

=cut

sub jmp {
	my ($self, $label)= @_;
	use integer;
	$label= $self->get_label($label)
		unless ref $label;
	my $ofs;
	my $short;
	$self->_mark_unresolved(
		2, # estimated length
		calc_size => sub {
			my ($self, $params)= @_;
			defined $label->{start} or croak "Label $label is not marked";
			$ofs= $label->{start} - ($params->{start}+$params->{len});
			$short= (($ofs>>7) == ($ofs>>8)); 
			return $short? 2 : 5;
		},
		encode => sub {
			# We rely on the calc_size() getting called before encode
			return $short?
				pack('Cc', 0xEB, $ofs)
				: pack('CV', 0xE9, $ofs);
		}
	);
	$self;
}

=item jmp_abs

Jump to the absolute address contained in a register.

=cut

sub jmp_abs_reg {
	my ($self, $reg)= @_;
	$self->_append_reg_reg(0, 0xFF, 4, $reg);
}

=item jmp_abs_mem

Jump to the absolute address read from a memory location

=cut

sub jmp_abs_mem {
	my ($self, $base_reg, $disp, $index_reg, $scale)= @_;
	$self->_append_reg_mem(0, 0xFF, 4, $base_reg, $disp, $index_reg, $scale);
}

=item jmp_if_eq, je, jz

=item jmp_if_ne, jne, jnz

Jump to label if zero flag is/isn't set after CMP instruction

=cut

sub jmp_if_eq { shift->_append_jmp_cond(4, shift) }
*jz= *jmp_if_eq;
*je= *jmp_if_eq;

sub jmp_if_ne { shift->_append_jmp_cond(5, shift) }
*jne= *jmp_if_ne;
*jnz= *jmp_if_ne;

=item jmp_if_unsigned_lt, jb, jmp_if_carry, jc

=item jmp_if_unsigned_gt, ja

=item jmp_if_unsigned_le, jbe

=item jmp_if_unsigned_ge, jae, jmp_unless_carry, jnc

Jump to label if unsigned less-than / greater-than / less-or-equal / greater-or-equal

=cut

sub jmp_if_unsigned_lt { shift->_append_jmp_cond(2, shift) }
*jb= *jmp_if_unsigned_lt;
*jc= *jmp_if_unsigned_lt;

sub jmp_if_unsigned_gt { shift->_append_jmp_cond(7, shift) }
*ja= *jmp_if_unsigned_gt;

sub jmp_if_unsigned_le { shift->_append_jmp_cond(6, shift) }
*jbe= *jmp_if_unsigned_le;

sub jmp_if_unsigned_ge { shift->_append_jmp_cond(3, shift) }
*jae= *jmp_if_unsigned_ge;
*jnc= *jmp_if_unsigned_ge;

=item jmp_if_signed_lt, jl

=item jmp_if_signed_gt, jg

=item jmp_if_signed_le, jle

=item jmp_if_signed_ge, jge

Jump to label if signed less-than / greater-than / less-or-equal / greater-or-equal

=cut

sub jmp_if_signed_lt { shift->_append_jmp_cond(12, shift) }
*jl= *jmp_if_signed_lt;

sub jmp_if_signed_gt { shift->_append_jmp_cond(15, shift) }
*jg= *jmp_if_signed_gt;

sub jmp_if_signed_le { shift->_append_jmp_cond(14, shift) }
*jle= *jmp_if_signed_le;

sub jmp_if_signed_ge { shift->_append_jmp_cond(13, shift) }
*jge= *jmp_if_signed_ge;

=item jmp_if_sign, js

=item jmp_unless_sign, jns

Jump to label if 'sign' flag is/isn't set after CMP instruction

=item jmp_if_overflow, jo

=item jmp_unless_overflow, jno

Jump to label if overflow flag is/isn't set after CMP instruction

=item jmp_if_parity_even, jpe, jp

=item jmp_if_parity_odd, jpo, jnp

Jump to label if 'parity' flag is/isn't set after CMP instruction

=cut

sub jmp_if_sign         { shift->_append_jmp_cond(8, shift) }
*js= *jmp_if_sign;

sub jmp_unless_sign     { shift->_append_jmp_cond(9, shift) }
*jns= *jmp_unless_sign;

sub jmp_if_overflow     { shift->_append_jmp_cond(0, shift) }
*jo= *jmp_if_overflow;

sub jmp_unless_overflow { shift->_append_jmp_cond(1, shift) }
*jno= *jmp_unless_overflow;

sub jmp_if_parity_even  { shift->_append_jmp_cond(10, shift) }
*jpe= *jmp_if_parity_even;
*jp= *jmp_if_parity_even;

sub jmp_if_parity_odd   { shift->_append_jmp_cond(11, shift) }
*jpo= *jmp_if_parity_odd;
*jnp= *jmp_if_parity_odd;

=item jmp_cx_zero, jrcxz

Short-jump to label if RCX register is zero

=item loop

Decrement RCX and short-jump to label if RCX register is nonzero
(decrement of RCX does not change rFLAGS)

=item loopz, loope

Decrement RCX and short-jump to label if RCX register is nonzero and zero flag (ZF) is set.
(decrement of RCX does not change rFLAGS)

=item loopnz, loopne

Decrement RCX and short-jump to label if RCX register is nonzero and zero flag (ZF) is not set
(decrement of RCX does not change rFLAGS)

=cut

sub jmp_cx_zero { shift->_append_jmp_cx(0xE3, shift) }
*jrcxz= *jmp_cx_zero;

sub loop        { shift->_append_jmp_cx(0xE2, shift) }

sub loopz       { shift->_append_jmp_cx(0xE1, shift) }
*loope= *loopz;

sub loopnz      { shift->_append_jmp_cx(0xE0, shift) }
*loopne= *loopnz;

=item syscall

Syscall instruction, takes no arguments.  (params are stored in pre-defined registers)

=cut

sub syscall {
	$_[0]{_buf} .= "\x0F\x05";
	$_[0];
}

=head1 x86_64 INSTRUCTIONS

The AMD64 Architecture Programmer's Manual is a somewhat tedious read, so here's my notes:

Typical 2-arg 64-bit instruction:
	REX ( AddrSize ) Opcode ModRM ( ScaleIndexBase ( Disp ) ) ( Immed )

	REX: use extended registers and/or 64-bit operand sizes.
		Not used for simple push/pop or handful of others
	REX = 0x40 + (W:1 R:1 X:1 B:1)
		REX.W = "wide" (64-bit operand size when set)
		REX.R is 4th bit of ModRM.Reg
		REX.X is 4th bit of SIB.Index
		REX.B is 4th bit of ModRM.R/M or of SIB.Base or of ModRM.Reg depending on goofy rules
  
	ModRM: mode/registers flags
	ModRM = (Mod:2 Reg:3 R/M:3)
		ModRM.Mod indicates operands:
			11b means ( Reg, R/M-reg-value )
			00b means ( Reg, R/M-reg-addr ) unless second reg is SP/BP/R12/R13
			01b means ( Reg, R/M-reg-addr + 8-bit disp ) unless second reg is SP/R12
			10b means ( Reg, R/M-reg-addr + 32-bit disp ) unless second reg is SP/R12
			
			When using addrs, R/M=100b means include the SIB byte for exotic addressing options
			In the 00b case, R/M=101b means use instruction pointer + 32-bit immed

	SIB: optional byte for wild and crazy memory addressing; activate with ModRM.R/M = 0100b
	SIB = (Scale:2 Index:3 Base:3)
		address is (index_register << scale) + base_register (+immed per the ModRM.Mod bits)
		* unless index_register = 0100b then no register is used.
			(i.e. RSP cannot be used as an index register )
		* unless base_register = _101b and ModRM.mod = 00 then no register is used.
			(i.e. [R{BP,13} + R?? * 2] must be written as [R{BP,13} + R?? * 2 + 0]

=head1 UTILITY METHODS FOR ENCODING INSTRUCTIONS

=head2 _append_reg_reg

Encode standard 64-bit instruction with REX prefix which refers only to registers.
This skips all the memory addressing logic since it is only operating on registers,
and always produces known-length encodings.

=cut

# For every register and alias, this maps to the 3-bit code identifying the register
# in the 32-bit instruction set
# TODO: I don't know enough about the 16-bit instruction set, and left them out.
my %regnum= (
	RAX => 0, RCX => 1, RDX => 2, RBX => 3,
	RSP => 4, RBP => 5, RSI => 6, RDI => 7,
	R0  => 0, R1  => 1, R2  => 2, R3  => 3,
	R4  => 4, R5  => 5, R6  => 6, R7  => 7,
	R8  => 0, R9  => 1, R10 => 2, R11 => 3,
	R12 => 4, R13 => 5, R14 => 6, R15 => 7,
	map { $_ => ($_&0x7) } 0..15
);
$regnum{lc $_}= $regnum{$_} for keys %regnum;

# For every register and alias, this maps to the bit of the 64-bit instruction
# that indicates whether it is a classic register or new register.
# TODO: I don't know enough about the 16-bit instruction set, and left them out.
my %regext= (
	RAX => 0, RCX => 0, RDX => 0, RBX => 0,
	RSP => 0, RBP => 0, RSI => 0, RDI => 0,
	R0  => 0, R1  => 0, R2  => 0, R3  => 0,
	R4  => 0, R5  => 0, R6  => 0, R7  => 0,
	R8  => 1, R9  => 1, R10 => 1, R11 => 1,
	R12 => 1, R13 => 1, R14 => 1, R15 => 1,
	map { $_ => ($_>>3) } 0..15
);
$regext{lc $_}= $regext{$_} for keys %regext;

sub _append_reg_reg {
	my ($self, $rex, $opcode, $reg1, $reg2)= @_;
	use integer;
	
	my $r1_num= $regnum{$reg1} // croak "$reg1 not a register";
	$rex |= $regext{$reg1} << 2;
	
	my $r2_num= $regnum{$reg2} // croak "$reg2 not a register";
	$rex |= $regext{$reg2};
	
	$self->{_buf} .= $rex?
		pack('CCC', 0x40|$rex, $opcode, 0xC0 + ($r1_num << 3) + $r2_num)
		: pack('CC', $opcode, 0xC0 + ($r1_num << 3) + $r2_num);
	$self;
}

=head2 _append_reg_mem

Encode standard 64-bit instruction with REX prefix which addresses memory for one of its operands.
The encoded length might not be resolved until later if an unknown displacement value was given.

=cut

my %regnum_not_sp= map { $_ => $regnum{$_} } grep { !/rsp|r4/i } keys %regnum;

# scale values for the SIB byte
my %SIB_scale= (
	1 => 0x00,
	2 => 0x40,
	4 => 0x80,
	8 => 0xC0
);

sub _append_reg_mem {
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	use integer;
	my $r1_num= $regnum{$reg} // croak "invalid register $reg";
	$rex |= $regext{$reg} << 2;
	
	my ($base_num, $index_num);
	my $disp_unknown;
	
	if (ref $disp) {
		if (defined $disp->value) {
			$disp= $disp->value;
		} else {
			# Just force it to 32-bit rather than making this instruction an unknown length
			$disp->bits(32) unless $disp->bits;
			$disp_unknown= $disp;
			$disp= (-1 << ($disp->bits-1));
		}
	}
	
	my $tail;
	if (defined $base_reg) {
		$base_num= $regnum{$base_reg} // croak "invalid base register $base_reg";
		$rex |= $regext{$base_reg};
		
		# RBP always gets mod_rm displacement to differentiate from Null base register
		my ($mod_rm, $suffix)= !$disp? ( $base_num == 5? (0x40, "\0") : (0x00, '') )
			: (($disp >>  7) == ($disp >>  8))? (0x40, chr($disp & 0xFF))
			: (($disp >> 31) == ($disp >> 32))? (0x80, pack('V', $disp))
			: croak "address displacement out of range: $disp";
		
		if (defined $index_reg) {
			my $scale= $SIB_scale{$scale // 1} // croak "invalid index multiplier $scale";
			my $index_num= $regnum_not_sp{$index_reg} // croak "invalid index register $index_reg";
			$rex |= $regext{$index_reg} << 1;
			$tail= pack('CC', $mod_rm | ($r1_num << 3) | 4, $scale | ($index_num << 3) | $base_num) . $suffix;
		}
		# RSP always gets a SIB byte
		elsif ($base_num == 4) {
			$tail= pack('CC', $mod_rm | ($r1_num << 3) | 4, 0x24) . $suffix;
		}
		else {
			$tail= pack('C', $mod_rm | ($r1_num << 3) | $base_num) . $suffix;
		}
	} else {
		# Null base register is encoded as RBP + 32bit displacement
		$base_num= 5;
		(($disp >> 31) == ($disp >> 32))
			or croak "address displacement out of range: $disp";
		
		if (defined $index_reg) {
			my $scale= $SIB_scale{$scale // 1} // croak "invalid index multiplier $scale";
			my $index_num= $regnum_not_sp{$index_reg} // croak "invalid index register $index_reg";
			$rex |= $regext{$index_reg} << 1;
			$tail= pack('CCV', ($r1_num << 3) | 4, $scale | ($index_num << 3) | $base_num, $disp);
		}
		else {
			$tail= pack('CCV', ($r1_num << 3) | 4, 0x20 | $base_num, $disp);
		}
	}
	
	$self->{_buf} .= $rex?
		pack('CC', ($rex|0x40), $opcode) . $tail
		: pack('C', $opcode) . $tail;
	
	if ($disp_unknown) {
		if ($disp_unknown->bits <= 8) {
			$self->_mark_unresolved(-1, value => $disp_unknown, encode => '_repack8' );
		} else {
			$self->_mark_unresolved(-4, value => $disp_unknown, encode => '_repack32' );
		}
	}
	
	return $self;
}

sub _repack32 {
	my ($self, $params)= @_;
	my $v= $params->{value}->value;
	defined $v or croak "Placeholder $params->{value} has not been assigned";
	return pack('V', $v);
}

=head2 _encode_jmp_cond

Encodes a conditional jump instruction, which is either the short 2-byte form for 8-bit offsets,
or 6 bytes for jumps of 32-bit offsets.

=cut

sub _append_jmp_cond {
	my ($self, $cond, $label)= @_;
	use integer;
	$label= $self->get_label($label)
		unless ref $label;
	my $ofs;
	my $short;
	$self->_mark_unresolved(
		2, # estimated length
		calc_size => sub {
			my ($self, $params)= @_;
			defined $label->{start} or croak "Label $label is not marked";
			$ofs= $label->{start} - ($params->{start}+$params->{len});
			$short= (($ofs>>7) == ($ofs>>8));
			#print "jmp ofs=$ofs short=$short\n";
			return $short? 2 : 6;
		},
		encode => sub {
			# We rely on the calc_size() getting called before encode
			return $short?
				pack('Cc', 0x70 + $cond, $ofs)
				: pack('CCV', 0x0F, 0x80 + $cond, $ofs);
		}
	);
	$self;
}

sub _append_jmp_cx {
	my ($self, $op, $label)= @_;
	use integer;
	$label= $self->get_label($label)
		unless ref $label;
	my $ofs;
	my $short;
	$self->_mark_unresolved(
		2, # estimated length
		encode => sub {
			my ($self, $params)= @_;
			defined $label->{start} or croak "Label $label is not marked";
			my $ofs= $label->{start} - ($params->{start}+$params->{len});
			(($ofs>>7) == ($ofs>>8)) or croak "Label too far, can only short-jump";
			return pack('Cc', $op, $ofs);
		}
	);
	return $self;
}

sub _encode_mov64_imm {
	my ($self, $reg, $immed)= @_;
	my $dst_num= $regnum{$reg} // die "$reg is not a 64-bit register";
	my $dst_ext= $regext{$reg};
	use integer;
	# If the number fits in 32-bits, encode as the classic instruction
	if (!($immed >> 32)) {
		return $dst_ext?
			pack('CCL<', 0x41, 0xB8 + $dst_num, $immed)
			: pack('CL<', 0xB8 + $dst_num, $immed);
	}
	# If the number can sign-extend from 32-bits, encode as 32-bit sign-extend
	elsif (($immed >> 32) == -1) {
		return pack('CCCl<', 0x48 + $dst_ext, 0xC7, 0xC0 + $dst_num, $immed);
	}
	# else encode as new 64-bit immediate
	else {
		return pack('CCQ<', 0x48 + $dst_ext, 0xB8 + $dst_num, $immed);
	}
}

sub _mark_unresolved {
	my ($self, $location)= (shift, shift);
	my $start= length($self->{_buf});
	
	# If location is negative, move the 'start' back that many bytes.
	# The length is the abs of location.
	if ($location < 0) {
		$location= -$location;
		$start -= $location;
	}
	# If the location is positive, start is the end of the string.
	# Add padding bytes for the length of the instruction.
	else {
		$self->{_buf} .= "\0" x $location;
	}
	
	#print "Unresolved at $start ($location)\n";
	push @{ $self->_unresolved }, { start => $start, len => $location, @_ };
}

sub _resolve {
	my $self= shift;
	
	# Start by making sure every instruction is the length it needs to be
	my $changed_len= 1;
	while ($changed_len) {
		$changed_len= 0;
		my $ofs= 0;
		for my $p (@{ $self->_unresolved }) {
			#print "Shifting $p by $ofs\n" if $ofs;
			$p->{start} += $ofs if $ofs;
			my $fn= $p->{calc_size}
				or next;
			my $result= $self->$fn($p);
			if ($result != $p->{len}) {
				#print "New size is $result\n";
				$changed_len= 1;
				$ofs += ($result - $p->{len});
				$p->{len}= $result;
			}
		}
	}
	
	# Then encode each instruction
	my $len_check= length($self->{_buf});
	for my $p (@{ $self->_unresolved }) {
		my $fn= $p->{encode}
			or next;
		#print "replace $p->{start}, $p->{len} with $fn\n";
		substr($self->{_buf}, $p->{start}, $p->{len})= $self->$fn($p);
	}
	$len_check == length($self->{_buf})
		or die 'An instruction changed length during encode()';
	
	# Clear the list
	@{ $self->_unresolved }= ();
}

1;
