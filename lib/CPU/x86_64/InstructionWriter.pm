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

# Map 64-bit register names to the numeric register number
my %regnum64= (
	RAX => 0, RCX => 1, RDX => 2, RBX => 3,
	rax => 0, rcx => 1, rdx => 2, rbx => 3,
	RSP => 4, RBP => 5, RSI => 6, RDI => 7,
	rsp => 4, rbp => 5, rsi => 6, rdi => 7,
	map { $_ => $_, "R$_" => $_, "r$_" => $_ } 0..15
);

my %regnum32= (
	EAX => 0, ECX => 1, EDX => 2, EBX => 3,
	eax => 0, ecx => 1, edx => 2, ebx => 3,
	RSP => 4, RBP => 5, RSI => 6, RDI => 7,
	rsp => 4, rbp => 5, rsi => 6, rdi => 7,
	map { $_ => $_, "R${_}D" => $_, "r${_}d" => $_ } 0..15
);

my %regnum16= (
	AX => 0, CX => 1, DX => 2, BX => 3,
	ax => 0, cx => 1, dx => 2, bx => 3,
	SP => 4, BP => 5, SI => 6, DI => 7,
	sp => 4, bp => 5, si => 6, di => 7,
	map { $_ => $_, "R${_}W" => $_, "r${_}w" => $_ } 0..15
);

my %regnum8= (
	AL => 0, CL => 1, DL => 2, BL => 3,
	al => 0, cl => 1, dl => 2, bl => 3,
	map { $_ => $_, "R${_}B" => $_, "r${_}b" => $_, "R${_}L" => $_, "r${_}l" => $_ } 0..15
);
my %regnum8_high= (
	AH => 4, CH => 5, DH => 6, BH => 7,
	ah => 4, ch => 5, dh => 6, bh => 7,
);

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

=head2 C<get_label()>, C<get_label($name)>

Return a label object for the given name, or if no name is given, return an anonymous label.

The label objects returned can be assigned a location within the instruction stream using L</mark>
and used as thetarget for C<JMP> and C<JMP>-like instructions.  A label can also be used as a
constant once all variable-length instructions have been L</resolve>d and once L</start_address>
is defined.

=cut

sub get_label {
	my ($self, $name)= @_;
	my $labels= $self->labels;
	$name //= '__auto_'.(scalar keys %$labels);
	$labels->{$name} //= { name => $name };
}

=head2 C<mark($label_name)>, C<mark($undef_var)>, C<mark($label_ref)>

Bind a named label to the current position in the instruction buffer.  You can also pass a label
reference from L</get_label>, or an undef variable which will be assigned a label.

If the current position follows instructions of unknown length, the label will be processed as an
unknown, and shift automatically as the instructions are resolved.

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

=head2 MOV

=over

=item C<mov64_from_reg($dest_reg, $src_reg)>

Copy second register to first register.  Copies full 64-bit value.

=cut

sub mov64_from_reg {
	$_[0]{_buf} .= $_[0]->_encode_op_reg_reg(0x08, 0x89,
		$regnum64{$_[2]} // croak("$_[2] is not a 64-bit register"),
		$regnum64{$_[1]} // croak("$_[1] is not a 64-bit register"),
	);
	$_[0];
}

=item C<mov64_to_mem($reg, $base_reg, $offset, $index_reg, $index_scale)>

Copy 64-bit value in register (first param) to a L</memory location>.

=item C<mov64_from_mem($reg, $base_reg, $offset, $index_reg, $index_scale)>

Copy 64-bit value at L</memory location> (second params) into register (first param).

=cut

sub mov64_to_mem   { shift->_append_op_reg64_mem(0x08, 0x89, @_); }
sub mov64_from_mem { shift->_append_op_reg64_mem(0x08, 0x8B, @_); }

sub mov32_to_mem   { shift->_append_op_reg32_mem(0, 0x89, @_); }
sub mov32_from_mem { shift->_append_op_reg32_mem(0, 0x8B, @_); }

sub mov16_to_mem   { shift->_append_op_reg16_mem(0, 0x89, @_); }
sub mov16_from_mem { shift->_append_op_reg16_mem(0, 0x8B, @_); }

sub mov8_to_mem    { shift->_append_op_reg8_mem(0, 0x88, @_); }
sub mov8_from_mem  { shift->_append_op_reg8_mem(0, 0x8A, @_); }

=item C<mov64_const($dest_reg, $constant)>

Load a constant value into a 64-bit register.  Constant is sign-extended to 64-bits.

=cut

sub mov64_imm {
	my ($self, $reg, $immed)= @_;
	$reg= $regnum64{$reg} // croak("$reg is not a 64-bit register");
	use integer;
	if (ref $immed) {
		return $self->_mark_unresolved(
			10,
			encode => sub {
				my $value= $immed->value
					// croak "$immed is not a defined value";
				shift->_encode_mov64_imm($reg, $value);
			}
		);
	}
	$self->{_buf} .= $self->_encode_mov64_imm($reg, $immed);
	$self;
}

=back

=head2 JMP

All jump instructions are relative, and take either a numeric offset (from the start of the next
instruction) or a label, except the C<jmp_abs> instruction which takes a register containing the
target address, and the C<jmp_from_addr> which reads a memory address for the address to jump to.

=over

=item jmp

Unconditional jump to label (or 32-bit offset constant).

=cut

sub jmp {
	@_ == 2 or croak "Wrong arguments";
	$_[1]= $_[0]->get_label
		unless defined $_[1];
	my ($self, $label)= @_;
	use integer;
	$label= $self->get_label($label)
		unless ref $label;
	$self->_mark_unresolved(
		2, # estimated length
		encode => sub {
			my ($self, $params)= @_;
			defined $label->{start} or croak "Label $label is not marked";
			my $ofs= $label->{start} - ($params->{start}+$params->{len});
			my $short= (($ofs>>7) == ($ofs>>8)); 
			# We rely on the calc_size() getting called before encode
			return $short?
				pack('Cc', 0xEB, $ofs)
				: pack('CV', 0xE9, $ofs);
		}
	);
	$self;
}

=item C<jmp_abs($reg)>

Jump to the absolute address contained in a register.

=cut

sub jmp_abs_reg {
	my ($self, $reg)= @_;
	$self->{_buf} .= $self->_encode_op_reg_reg(0, 0xFF, 4,
		$regnum64{$reg} // croak("$reg is not a 64-bit register"),
	);
	$self;
}

=item C<jmp_abs_mem($base_reg, $displacement, $index_reg, $scale)>

Jump to the absolute address read from a memory location

=cut

sub jmp_abs_mem {
	#my ($self, $base_reg, $disp, $index_reg, $scale)= @_;
	shift->_append_op_reg64_mem(0, 0xFF, 4, @_);
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

=item mfence, lfence, sfence

Parameterless instructions for memory access serialization.
Forces memory operations before the fence to compete before memory operations after the fence.
Lfence affects load operations, sfence affects store operations, and mfence affects both.

=cut

sub mfence {
	$_[0]{_buf} .= "\x0F\xAE\xF0";
	$_[0];
}
sub lfence {
	$_[0]{_buf} .= "\x0F\xAE\xE8";
	$_[0];
}
sub sfence {
	$_[0]{_buf} .= "\x0F\xAE\xF8";
	$_[0];
}

=head1 ENCODING x86_64 INSTRUCTIONS

The AMD64 Architecture Programmer's Manual is a somewhat tedious read, so here's my notes:

Typical 2-arg 64-bit instruction:
	REX ( AddrSize ) Opcode ModRM ( ScaleIndexBase ( Disp ) ) ( Immed )

	REX: use extended registers and/or 64-bit operand sizes.
		Not used for simple push/pop or handful of others
	REX = 0x40 + (W:1bit R:1bit X:1bit B:1bit)
		REX.W = "wide" (64-bit operand size when set)
		REX.R is 4th bit of ModRM.Reg
		REX.X is 4th bit of SIB.Index
		REX.B is 4th bit of ModRM.R/M or of SIB.Base or of ModRM.Reg depending on goofy rules
  
	ModRM: mode/registers flags
	ModRM = (Mod:2bit Reg:3bit R/M:3bit)
		ModRM.Mod indicates operands:
			11b means ( Reg, R/M-reg-value )
			00b means ( Reg, R/M-reg-addr ) unless second reg is SP/BP/R12/R13
			01b means ( Reg, R/M-reg-addr + 8-bit disp ) unless second reg is SP/R12
			10b means ( Reg, R/M-reg-addr + 32-bit disp ) unless second reg is SP/R12
			
			When accessing mem, R/M=100b means include the SIB byte for exotic addressing options
			In the 00b case, R/M=101b means use instruction pointer + 32-bit immed

	SIB: optional byte for wild and crazy memory addressing; activate with ModRM.R/M = 0100b
	SIB = (Scale:2bit Index:3bit Base:3bit)
		address is (index_register << scale) + base_register (+immed per the ModRM.Mod bits)
		* unless index_register = 0100b then no register is used.
			(i.e. RSP cannot be used as an index register )
		* unless base_register = _101b and ModRM.mod = 00 then no register is used.
			(i.e. [R{BP,13} + R?? * 2] must be written as [R{BP,13} + R?? * 2 + 0]

=head1 UTILITY METHODS FOR ENCODING INSTRUCTIONS

=head2 _encode_op_reg_reg

Encode standard instruction with REX prefix which refers only to registers.
This skips all the memory addressing logic since it is only operating on registers,
and always produces known-length encodings.

=cut

sub _encode_op_reg_reg {
	my ($self, $rex, $opcode, $reg1, $reg2)= @_;
	use integer;
	$rex |= (($reg1 & 8) >> 1) | (($reg2 & 8) >> 3);
	return $rex?
		pack('CCC', 0x40|$rex, $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7))
		: pack('CC', $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7));
}

=head2 _append_op_reg64_mem

Encode standard 64-bit instruction with REX prefix which addresses memory for one of its operands.
The encoded length might not be resolved until later if an unknown displacement value was given.

=cut

sub _append_op_reg64_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$reg= $regnum64{$reg} // croak "$reg is not a valid 64-bit register"
		if defined $reg;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	if (ref $disp) {
		$self->_mark_unresolved(
			7, # estimated length
			encode => sub {
				defined $disp->value or croak "Unresolved displacement $disp";
				shift->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp->value, $index_reg, $scale)
			},
		);
	}
	else {
		$self->{_buf} .= $self->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale);
	}
	$self;
}

sub _append_op_reg32_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$reg= $regnum32{$reg} // croak "$reg is not a valid 32-bit register"
		if defined $reg;
	$base_reg= $regnum32{$base_reg} // croak "$base_reg is not a valid 32-bit register"
		if defined $base_reg;
	$index_reg= $regnum32{$index_reg} // croak "$index_reg is not a valid 32-bit register"
		if defined $index_reg;
	if (ref $disp) {
		$self->_mark_unresolved(
			7, # estimated length
			encode => sub {
				defined $disp->value or croak "Unresolved displacement $disp";
				shift->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp->value, $index_reg, $scale)
			},
		);
	}
	else {
		$self->{_buf} .= $self->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale);
	}
	$self;
}

sub _append_op_reg16_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$reg= $regnum16{$reg} // croak "$reg is not a valid 16-bit register"
		if defined $reg;
	$base_reg= $regnum16{$base_reg} // croak "$base_reg is not a valid 16-bit register"
		if defined $base_reg;
	$index_reg= $regnum16{$index_reg} // croak "$index_reg is not a valid 16-bit register"
		if defined $index_reg;
	if (ref $disp) {
		$self->_mark_unresolved(
			7, # estimated length
			encode => sub {
				defined $disp->value or croak "Unresolved displacement $disp";
				"\x66".shift->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp->value, $index_reg, $scale)
			},
		);
	}
	else {
		$self->{_buf} .= "\x66".$self->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale);
	}
	$self;
}

sub _append_op_reg8_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum8{$base_reg} // croak "$base_reg is not a valid 8-bit register"
		if defined $base_reg;
	$index_reg= $regnum8{$index_reg} // croak "$index_reg is not a valid 8-bit register"
		if defined $index_reg;
	$reg= $regnum8{$reg};
	# special case for the "high byte" registers
	if (!defined $reg) {
		$reg= $regnum8_high{$_[3]} // croak "$_[3] is not a valid 8-bit register";
		!$rex && ($base_reg//0) < 8 && ($index_reg//0) < 8
			or croak "Cannot use $_[3] in instruction with REX prefix";
	}
	# special case for needing REX byte for SPL, BPL, DIL, and SIL
	elsif ($reg > 3) {
		$rex |= 0x40;
	}
	
	if (ref $disp) {
		$self->_mark_unresolved(
			7, # estimated length
			encode => sub {
				defined $disp->value or croak "Unresolved displacement $disp";
				shift->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp->value, $index_reg, $scale)
			},
		);
	}
	else {
		$self->{_buf} .= $self->_encode_op_reg_mem($rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale);
	}
	$self;
}

# scale values for the SIB byte
my %SIB_scale= (
	1 => 0x00,
	2 => 0x40,
	4 => 0x80,
	8 => 0xC0
);

sub _encode_op_reg_mem {
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	use integer;
	$rex |= ($reg & 8) >> 1;
	
	my $tail;
	if (defined $base_reg) {
		$rex |= ($base_reg & 8) >> 3;
		
		# RBP,R13 always gets mod_rm displacement to differentiate from Null base register
		my ($mod_rm, $suffix)= !$disp? ( ($base_reg&7) == 5? (0x40, "\0") : (0x00, '') )
			: (($disp >>  7) == ($disp >>  8))? (0x40, pack('c', $disp))
			: (($disp >> 31) == ($disp >> 32))? (0x80, pack('V', $disp))
			: croak "address displacement out of range: $disp";
		
		if (defined $index_reg) {
			my $scale= $SIB_scale{$scale // 1} // croak "invalid index multiplier $scale";
			$index_reg != 4 or croak "RSP cannot be used as index register";
			$rex |= ($index_reg & 8) >> 2;
			$tail= pack('CC', $mod_rm | (($reg & 7) << 3) | 4, $scale | (($index_reg & 7) << 3) | ($base_reg & 7)) . $suffix;
		}
		# RSP,R12 always gets a SIB byte
		elsif (($base_reg&7) == 4) {
			$tail= pack('CC', $mod_rm | (($reg & 7) << 3) | 4, 0x24) . $suffix;
		}
		else {
			# Null index register is encoded as RSP
			$tail= pack('C', $mod_rm | (($reg & 7) << 3) | ($base_reg & 7)) . $suffix;
		}
	} else {
		# Null base register is encoded as RBP + 32bit displacement
		
		(($disp >> 31) == ($disp >> 32))
			or croak "address displacement out of range: $disp";
		
		if (defined $index_reg) {
			my $scale= $SIB_scale{$scale // 1} // croak "invalid index multiplier $scale";
			$index_reg != 4 or croak "RSP cannot be used as index register";
			$rex |= ($index_reg & 8) >> 2;
			$tail= pack('CCV', (($reg & 7) << 3) | 4, $scale | (($index_reg & 7) << 3) | 5, $disp);
		}
		else {
			# Null index register is encoded as RSP
			$tail= pack('CCV', (($reg & 7) << 3) | 4, 0x25, $disp);
		}
	}
	
	return $rex?
		pack('CC', ($rex|0x40), $opcode) . $tail
		: pack('C', $opcode) . $tail;
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
	$_[2]= $_[0]->get_label unless defined $_[2];
	
	my ($self, $cond, $label)= @_;
	use integer;
	$label= $self->get_label($label)
		unless ref $label;
	$self->_mark_unresolved(
		2, # estimated length
		encode => sub {
			my ($self, $params)= @_;
			defined $label->{start} or croak "Label $label is not marked";
			my $ofs= $label->{start} - ($params->{start}+$params->{len});
			my $short= (($ofs>>7) == ($ofs>>8));
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
	use integer;
	# If the number fits in 32-bits, encode as the classic instruction
	if (!($immed >> 32)) {
		return $reg > 7? # need REX byte if extended register
			pack('CCL<', 0x41, 0xB8 + ($reg&7), $immed)
			: pack('CL<', 0xB8 + $reg, $immed);
	}
	# If the number can sign-extend from 32-bits, encode as 32-bit sign-extend
	elsif (($immed >> 32) == -1) {
		return pack('CCCl<', 0x48 | (($reg & 8) >> 3), 0xC7, 0xC0 + ($reg & 7), $immed);
	}
	# else encode as new 64-bit immediate
	else {
		return pack('CCQ<', 0x48 | (($reg & 8) >> 3), 0xB8 + ($reg & 7), $immed);
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
			my $fn= $p->{encode}
				or next;
			my $enc= $p->{encoded}= $self->$fn($p);
			substr($self->{_buf}, $p->{start}, $p->{len})= $enc;
			if (length($enc) != $p->{len}) {
				#print "New size is $result\n";
				$changed_len= 1;
				$ofs += (length($enc) - $p->{len});
				$p->{len}= length($enc);
			}
		}
	}
	
	# Clear the list
	@{ $self->_unresolved }= ();
}

1;
