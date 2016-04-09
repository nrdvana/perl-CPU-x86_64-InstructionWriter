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
	ESP => 4, EBP => 5, ESI => 6, EDI => 7,
	esp => 4, ebp => 5, esi => 6, edi => 7,
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
	SPL => 4, BPL => 5, SIL => 6, DIL => 7,
	spl => 4, bpl => 5, sil => 6, dil => 7,
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

=head2 NOP

Insert one or more no-op instructions.

=over

=item nop(), C<nop( $n )>

If called without an argument, insert one no-op.  Else insert C<$n> no-ops.

=cut

sub nop {
	$_[0]{_buf} .= (defined $_[1]? "\x90" x $_[1] : "\x90");
	$_[0];
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
	shift->_append_op64_reg_mem(0, 0xFF, 4, @_);
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

=back

=cut

sub jmp_cx_zero { shift->_append_jmp_cx(0xE3, shift) }
*jrcxz= *jmp_cx_zero;

sub loop        { shift->_append_jmp_cx(0xE2, shift) }

sub loopz       { shift->_append_jmp_cx(0xE1, shift) }
*loope= *loopz;

sub loopnz      { shift->_append_jmp_cx(0xE0, shift) }
*loopne= *loopnz;

=head2 MOV

=over

=item C<mov64_reg($dest_reg, $src_reg)>

Copy second register to first register.  Copies full 64-bit value.

=cut

sub mov64_reg { shift->_append_op64_reg_reg(0x89, $_[1], $_[0]) }

=item C<mov64_to_mem($reg, $base_reg, $offset, $index_reg, $index_scale)>

Copy 64-bit value in register (first param) to a L</memory location>.

=item C<mov64_from_mem($reg, $base_reg, $offset, $index_reg, $index_scale)>

Copy 64-bit value at L</memory location> (second params) into register (first param).

=cut

sub mov64_to_mem   { shift->_append_op64_reg_mem(8, 0x89, @_); }
sub mov32_to_mem   { shift->_append_op32_reg_mem(0, 0x89, @_); }
sub mov16_to_mem   { shift->_append_op16_reg_mem(0, 0x89, @_); }
sub mov8_to_mem    { shift->_append_op8_reg_mem(0, 0x88, @_); }

sub mov64_mem { shift->_append_op64_reg_mem(8, 0x8B, @_); }
sub mov32_mem { shift->_append_op32_reg_mem(0, 0x8B, @_); }
sub mov16_mem { shift->_append_op16_reg_mem(0, 0x8B, @_); }
sub mov8_mem  { shift->_append_op8_reg_mem(0, 0x8A, @_); }

=item C<mov64_const($dest_reg, $constant)>

Load a constant value into a 64-bit register.  Constant is sign-extended to 64-bits.

=cut

sub mov64_const {
	my ($self, $reg, $immed)= @_;
	$reg= $regnum64{$reg} // croak("$reg is not a 64-bit register");
	$self->_append_possible_unknown('_encode_mov64_imm', [$reg, $immed], 1, 10);
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
sub mov32_const {
	my ($self, $reg, $immed)= @_;
	$reg= $regnum32{$reg} // croak("$reg is not a 32-bit register");
	$self->{_buf} .= "\x41" if $reg > 7;
	$self->{_buf} .= pack('C' , 0xB8 | ($reg & 7));
	$self->_append_possible_unknown(sub { pack('V', $_[1]) }, [$immed], 0, 4);
}
sub mov16_const {
	my ($self, $reg, $immed)= @_;
	$reg= $regnum16{$reg} // croak("$reg is not a 16-bit register");
	$self->{_buf} .= "\x66";
	$self->{_buf} .= "\x41" if $reg > 7;
	$self->{_buf} .= pack('C', 0xB8 | ($reg & 7));
	$self->_append_possible_unknown(sub { pack('v', $_[1]) }, [$immed], 0, 2);
}
sub mov8_const {
	my ($self, $reg, $immed)= @_;
	$reg= $regnum8{$reg};
	# Special case for the high-byte registers available without the REX prefix
	if (!defined $reg) {
		$reg= $regnum8_high{$_[1]} // croak("$_[1] is not a 8-bit register");
	} else {
		$self->{_buf} .= pack('C', 0x40|(($reg&8)>>3)) if $reg > 3;
	}
	$self->{_buf} .= pack('C', 0xB0 | ($reg & 7));
	$self->_append_possible_unknown(sub { pack('C', $_[1]&0xFF) }, [$immed], 0, 1);
}

sub mov64_const_to_mem { shift->_append_op64_const_mem(0xC7, 0, @_) }
sub mov32_const_to_mem { shift->_append_op32_const_mem(0xC7, 0, @_) }
sub mov16_const_to_mem { shift->_append_op16_const_mem(0xC7, 0, @_) }
sub mov8_const_to_mem  { shift->_append_op8_const_mem (0xC6, 0, @_) }

=back

=head2 ADD

=over

=item C<add64_reg(reg64, reg64)>

=item C<add64_mem(reg64, base_reg64, displacement, index_reg64, scale)>

=item C<add64_to_mem(reg64, base_reg64, displacement, index_reg64, scale)>

=item C<add64_const(reg64, const)>

=item C<add64_const_to_mem(const, base_reg64, displacement, index_reg64, scale)>

=back

=cut

sub add64_reg { $_[0]->_append_op64_reg_reg(0x01, $_[2], $_[1]) }
sub add32_reg { $_[0]->_append_op32_reg_reg(0x01, $_[2], $_[1]) }
sub add16_reg { $_[0]->_append_op16_reg_reg(0x01, $_[2], $_[1]) }
sub add8_reg  { $_[0]->_append_op8_reg_reg (0x00, $_[2], $_[1]) }

sub add64_mem { shift->_append_op64_reg_mem(8, 0x03, @_); }
sub add32_mem { shift->_append_op32_reg_mem(0, 0x03, @_); }
sub add16_mem { shift->_append_op16_reg_mem(0, 0x03, @_); }
sub add8_mem  { shift->_append_op8_reg_mem (0, 0x02, @_); }

sub add64_to_mem { shift->_append_op64_reg_mem(8, 0x01, @_); }
sub add32_to_mem { shift->_append_op32_reg_mem(0, 0x01, @_); }
sub add16_to_mem { shift->_append_op16_reg_mem(0, 0x01, @_); }
sub add8_to_mem  { shift->_append_op8_reg_mem (0, 0x00, @_); }

sub add64_const { shift->_append_mathop64_const(0x05, 0x83, 0x81, 0, @_) }
sub add32_const { shift->_append_mathop32_const(0x05, 0x83, 0x81, 0, @_) }
sub add16_const { shift->_append_mathop16_const(0x05, 0x83, 0x81, 0, @_) }
sub add8_const  { shift->_append_mathop8_const (0x04, 0x80, 0, @_) }

sub add64_const_to_mem { shift->_append_mathop64_const_to_mem(0x83, 0x81, 0, @_) }
sub add32_const_to_mem { shift->_append_mathop32_const_to_mem(0x83, 0x81, 0, @_) }
sub add16_const_to_mem { shift->_append_mathop16_const_to_mem(0x83, 0x81, 0, @_) }
sub add8_const_to_mem  { shift->_append_mathop8_const_to_mem (0x80, 0, @_) }

=back

=head2 CMP

=over

=item cmp64_reg

=item cmp64_mem

=item cmp64_const

=cut

sub cmp64_reg { $_[0]->_append_op64_reg_reg(0x39, $_[2], $_[1]) }
sub cmp32_reg { $_[0]->_append_op32_reg_reg(0x39, $_[2], $_[1]) }
sub cmp16_reg { $_[0]->_append_op16_reg_reg(0x39, $_[2], $_[1]) }
sub cmp8_reg  { $_[0]->_append_op8_reg_reg (0x38, $_[2], $_[1]) }

sub cmp64_mem { shift->_append_op64_reg_mem(8, 0x3B, @_); }
sub cmp32_mem { shift->_append_op32_reg_mem(0, 0x3B, @_); }
sub cmp16_mem { shift->_append_op16_reg_mem(0, 0x3B, @_); }
sub cmp8_mem  { shift->_append_op8_reg_mem (0, 0x3A, @_); }

sub cmp64_to_mem { shift->_append_op64_reg_mem(8, 0x39, @_); }
sub cmp32_to_mem { shift->_append_op32_reg_mem(0, 0x39, @_); }
sub cmp16_to_mem { shift->_append_op16_reg_mem(0, 0x39, @_); }
sub cmp8_to_mem  { shift->_append_op8_reg_mem (0, 0x38, @_); }

sub cmp64_const { shift->_append_mathop64_const(0x3D, 0x83, 0x81, 7, @_) }
sub cmp32_const { shift->_append_mathop32_const(0x3D, 0x83, 0x81, 7, @_) }
sub cmp16_const { shift->_append_mathop16_const(0x3D, 0x83, 0x81, 7, @_) }
sub cmp8_const  { shift->_append_mathop8_const (0x3C, 0x80, 7, @_) }

sub cmp64_const_to_mem { shift->_append_mathop64_const_to_mem(0x83, 0x81, 7, @_) }
sub cmp32_const_to_mem { shift->_append_mathop32_const_to_mem(0x83, 0x81, 7, @_) }
sub cmp16_const_to_mem { shift->_append_mathop16_const_to_mem(0x83, 0x81, 7, @_) }
sub cmp8_const_to_mem  { shift->_append_mathop8_const_to_mem (0x80, 7, @_) }

=head2 DEC

=over

=item decNN_reg

=item decNN_mem

=back

=cut

sub dec64_reg { shift->_append_op64_reg_reg(0xFF, 1, @_) }
sub dec32_reg { shift->_append_op32_reg_reg(0xFF, 1, @_) }
sub dec16_reg { shift->_append_op16_reg_reg(0xFF, 1, @_) }
sub dec8_reg  { shift->_append_op8_reg_reg (0xFE, 1, @_) }

sub dec64_mem { shift->_append_op64_reg_mem(8, 0xFF, 1, @_) }
sub dec32_mem { shift->_append_op32_reg_mem(0, 0xFF, 1, @_) }
sub dec16_mem { shift->_append_op16_reg_mem(0, 0xFF, 1, @_) }
sub dec8_mem  { shift->_append_op8_reg_mem (0, 0xFE, 1, @_) }

=head2 INC

=over

=item incNN_reg

=item incNN_mem

=back

=cut

sub inc64_reg { shift->_append_op64_reg_reg(0xFF, 0, @_) }
sub inc32_reg { shift->_append_op32_reg_reg(0xFF, 0, @_) }
sub inc16_reg { shift->_append_op16_reg_reg(0xFF, 0, @_) }
sub inc8_reg  { shift->_append_op8_reg_reg (0xFE, 0, @_) }

sub inc64_mem { shift->_append_op64_reg_mem(8, 0xFF, 0, @_) }
sub inc32_mem { shift->_append_op32_reg_mem(0, 0xFF, 0, @_) }
sub inc16_mem { shift->_append_op16_reg_mem(0, 0xFF, 0, @_) }
sub inc8_mem  { shift->_append_op8_reg_mem (0, 0xFE, 0, @_) }

=head2 NOT

Flip all bits in a target register or memory location.

=over

=item notNN_reg

=item notNN_mem

=back

=cut

sub not64_reg { shift->_append_op64_reg_reg(0xF7, 2, @_) }
sub not32_reg { shift->_append_op32_reg_reg(0xF7, 2, @_) }
sub not16_reg { shift->_append_op16_reg_reg(0xF7, 2, @_) }
sub not8_reg  { shift->_append_op8_reg_reg (0xF6, 2, @_) }

sub not64_mem { shift->_append_op64_reg_mem(8, 0xF7, 2, @_) }
sub not32_mem { shift->_append_op32_reg_mem(0, 0xF7, 2, @_) }
sub not16_mem { shift->_append_op16_reg_mem(0, 0xF7, 2, @_) }
sub not8_mem  { shift->_append_op8_reg_mem (0, 0xF6, 2, @_) }

=head2 NEG

Replace target register or memory location with signed negation (2's complement).

=over

=item negNN_reg

=item negNN_mem

=back

=cut

sub neg64_reg { shift->_append_op64_reg_reg(0xF7, 3, @_) }
sub neg32_reg { shift->_append_op32_reg_reg(0xF7, 3, @_) }
sub neg16_reg { shift->_append_op16_reg_reg(0xF7, 3, @_) }
sub neg8_reg  { shift->_append_op8_reg_reg (0xF6, 3, @_) }

sub neg64_mem { shift->_append_op64_reg_mem(8, 0xF7, 3, @_) }
sub neg32_mem { shift->_append_op32_reg_mem(0, 0xF7, 3, @_) }
sub neg16_mem { shift->_append_op16_reg_mem(0, 0xF7, 3, @_) }
sub neg8_mem  { shift->_append_op8_reg_mem (0, 0xF6, 3, @_) }

=head2 DIV, IDIV

=over

=item divNNu_reg

Unsigned divide of _DX:_AX by a NN-bit register.  (divides AX into AL,AH for 8-bit) 

=item divNNu_mem

Unsigned divide of _DX:_AX by a NN-bit memory value referenced by 64-bit registers

=item divNNs_reg

Signed divide of _DX:_AX by a NN-bit register.  (divides AX into AL,AH for 8-bit)

=item divNNs_mem

Signed divide of _DX:_AX by a NN-bit memory value referenced by 64-bit registers

=back

=cut

sub div64u_reg { shift->_append_op64_reg_reg(0xF7, 6, @_) }
sub div32u_reg { shift->_append_op32_reg_reg(0xF7, 6, @_) }
sub div16u_reg { shift->_append_op16_reg_reg(0xF7, 6, @_) }
sub div8u_reg  { shift->_append_op8_opreg_reg(0xF6, 6, @_) }

sub div64u_mem { shift->_append_op64_reg_mem(8, 0xF7, 6, @_) }
sub div32u_mem { shift->_append_op32_reg_mem(0, 0xF7, 6, @_) }
sub div16u_mem { shift->_append_op16_reg_mem(0, 0xF7, 6, @_) }
sub div8u_mem  { shift->_append_op8_opreg_mem(0, 0xF6, 6, @_) }

sub div64s_reg { shift->_append_op64_reg_reg(0xF7, 7, @_) }
sub div32s_reg { shift->_append_op32_reg_reg(0xF7, 7, @_) }
sub div16s_reg { shift->_append_op16_reg_reg(0xF7, 7, @_) }
sub div8s_reg  { shift->_append_op8_opreg_reg(0xF6, 7, @_) }

sub div64s_mem { shift->_append_op64_reg_mem(8, 0xF7, 7, @_) }
sub div32s_mem { shift->_append_op32_reg_mem(0, 0xF7, 7, @_) }
sub div16s_mem { shift->_append_op16_reg_mem(0, 0xF7, 7, @_) }
sub div8s_mem  { shift->_append_op8_opreg_mem(0, 0xF6, 7, @_) }

=head2 flag modifiers

Each flag modifier takes an argument of 0 (clear), 1 (set), or -1 (invert).

=over

=item flag_carry($state), clc, cmc, stc

=item flag_direction($state), cld, std

=back

=cut

my @_carry_flag_op= ( "\xF5", "\xF8", "\xF9" );
sub flag_carry { $_[0]{_buf} .= $_carry_flag_op[$_[1] + 1]; $_[0] }
sub clc { $_[0]{_buf} .= "\xF8"; $_[0] }
sub cmc { $_[0]{_buf} .= "\xF5"; $_[0] }
sub stc { $_[0]{_buf} .= "\xF9"; $_[0] }

my @_direction_flag_op= ( "\xFC", "\xFD" );
sub flag_direction { $_[0]{_buf} .= $_direction_flag_op[$_[1]]; $_[0] }
sub cld { $_[0]{_buf} .= "\xFC"; $_[0] }
sub std { $_[0]{_buf} .= "\xFD"; $_[0] }

=head2 syscall

Syscall instruction, takes no arguments.  (params are stored in pre-defined registers)

=cut

sub syscall {
	$_[0]{_buf} .= "\x0F\x05";
	$_[0];
}

=head2 mfence, lfence, sfence

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

sub _append_op64_reg_reg {
	my ($self, $opcode, $reg1, $reg2)= @_;
	$reg1= ($regnum64{$reg1} // croak("$reg1 is not a 64-bit register"));
	$reg2= ($regnum64{$reg2} // croak("$reg2 is not a 64-bit register"));
	use integer;
	$self->{_buf} .= pack('CCC',
		0x48 | (($reg1 & 8) >> 1) | (($reg2 & 8) >> 3),
		$opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7));
	$self;
}
sub _append_op32_reg_reg {
	my ($self, $opcode, $reg1, $reg2)= @_;
	$reg1= ($regnum32{$reg1} // croak("$reg1 is not a 32-bit register"));
	$reg2= ($regnum32{$reg2} // croak("$reg2 is not a 32-bit register"));
	use integer;
	my $rex= (($reg1 & 8) >> 1) | (($reg2 & 8) >> 3);
	$self->{_buf} .= $rex?
		pack('CCC', 0x40|$rex, $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7))
		: pack('CC', $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7));
	$self;
}
sub _append_op16_reg_reg {
	my ($self, $opcode, $reg1, $reg2)= @_;
	$reg1= ($regnum16{$reg1} // croak("$reg1 is not a 16-bit register"));
	$reg2= ($regnum16{$reg2} // croak("$reg2 is not a 16-bit register"));
	use integer;
	my $rex= (($reg1 & 8) >> 1) | (($reg2 & 8) >> 3);
	$self->{_buf} .= $rex?
		pack('CCCC', 0x66, 0x40|$rex, $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7))
		: pack('CCC', 0x66, $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7));
	$self;
}
sub _append_op8_reg_reg {
	my ($self, $opcode, $reg1, $reg2)= @_;
	use integer;
	$reg1= $regnum8{$reg1};
	$reg2= $regnum8{$reg2};
	# special case for the "high byte" registers.  They can't be used in an
	# instruction that uses the REX prefix.
	if (!defined $reg1 || !defined $reg2) {
		my $old_reg1= $reg1;
		my $old_reg2= $reg2;
		$reg1= $regnum8_high{$_[2]} // croak "$_[2] is not a valid 8-bit register";
		$reg2= $regnum8_high{$_[3]} // croak "$_[3] is not a valid 8-bit register";
		if (($old_reg1 && $old_reg1 > 3) || ($old_reg2 && $old_reg2 > 3)) {
			croak "Can't combine $_[2] with $_[3] in same instruction"; 
		}
		$self->{_buf} .= pack('CC', $opcode, 0xC0 | ($reg1 << 3) | $reg2);
	}
	else {
		$self->{_buf} .= ($reg1 > 3 || $reg2 > 3)?
			pack('CCC', 0x40|(($reg1 & 8) >> 1) | (($reg2 & 8) >> 3), $opcode, 0xC0 | (($reg1 & 7) << 3) | ($reg2 & 7))
			: pack('CC', $opcode, 0xC0 | ($reg1 << 3) | $reg2);
	}
	$self;
}

# Like above, but the first register argument isn't really a register argument
# and therefore doesn't require a 0x40 prefix for values > 3
sub _append_op8_opreg_reg {
	my ($self, $opcode, $opreg, $reg2)= @_;
	use integer;
	$reg2= $regnum8{$reg2};
	# special case for the "high byte" registers.  They can't be used in an
	# instruction that uses the REX prefix.
	if (!defined $reg2) {
		my $old_reg2= $reg2;
		$reg2= $regnum8_high{$_[3]} // croak "$_[3] is not a valid 8-bit register";
		$self->{_buf} .= pack('CC', $opcode, 0xC0 | ($opreg << 3) | $reg2);
	}
	else {
		$self->{_buf} .= ($reg2 > 3)?
			pack('CCC', 0x40| (($reg2 & 8) >> 3), $opcode, 0xC0 | ($opreg << 3) | ($reg2 & 7))
			: pack('CC', $opcode, 0xC0 | ($opreg << 3) | $reg2);
	}
	$self;
}

=head2 _append_op64_reg_mem

Encode standard 64-bit instruction with REX prefix which addresses memory for one of its operands.
The encoded length might not be resolved until later if an unknown displacement value was given.

=cut

sub _append_op64_reg_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$reg= $regnum64{$reg} // croak "$reg is not a valid 64-bit register"
		if defined $reg;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->_append_possible_unknown('_encode_op_reg_mem', [$rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale], 4, 7);
	$self;
}

sub _append_op32_reg_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$reg= $regnum32{$reg} // croak "$reg is not a valid 32-bit register"
		if defined $reg;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->_append_possible_unknown('_encode_op_reg_mem', [$rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale], 4, 7);
}

sub _append_op16_reg_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$reg= $regnum16{$reg} // croak "$reg is not a valid 16-bit register"
		if defined $reg;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->{_buf} .= "\x66";
	$self->_append_possible_unknown('_encode_op_reg_mem', [$rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale], 4, 7);
}

sub _append_op8_reg_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
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
	$self->_append_possible_unknown('_encode_op_reg_mem', [$rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale], 4, 7);
}
# Like above, but the first register is a constant and don't need to test it for
# requiring a REX prefix if >3.
sub _append_op8_opreg_mem {
	@_ <= 8 or croak "Too many arguments";
	my ($self, $rex, $opcode, $opreg, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->_append_possible_unknown('_encode_op_reg_mem', [$rex, $opcode, $opreg, $base_reg, $disp, $index_reg, $scale], 4, 7);
}

# scale values for the SIB byte
my %SIB_scale= (
	1 => 0x00,
	2 => 0x40,
	4 => 0x80,
	8 => 0xC0
);

sub _encode_op_reg_mem {
	my ($self, $rex, $opcode, $reg, $base_reg, $disp, $index_reg, $scale, $immed_pack, $immed)= @_;
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
	$tail .= pack($immed_pack, $immed)
		if defined $immed;
	
	return $rex?
		pack('CC', ($rex|0x40), $opcode) . $tail
		: pack('C', $opcode) . $tail;
}

sub _append_op64_const_to_mem {
	my ($self, $opcode, $reg, $immed, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->_append_possible_unknown(
		'_encode_op_reg_mem',
		[ 8, $opcode, $reg, $base_reg, $disp, $index_reg, $scale, 'V', $immed ],
		4, 8, # indicies of might-be-unknown args
		defined $disp? 11 : 7 # estimated length
	);
}
sub _append_op32_const_to_mem {
	my ($self, $opcode, $reg, $immed, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->_append_possible_unknown(
		'_encode_op_reg_mem',
		[ 0, $opcode, $reg, $base_reg, $disp, $index_reg, $scale, 'V', $immed ],
		4, 8, # indicies of might-be-unknown args
		defined $disp? 11 : 7 # estimated length
	);
}
sub _append_op16_const_to_mem {
	my ($self, $opcode, $opcode_reg, $immed, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->{_buf} .= "\x66";
	$self->_append_possible_unknown(
		'_encode_op_reg_mem',
		[ 0, $opcode, $opcode_reg, $base_reg, $disp, $index_reg, $scale, 'v', $immed ],
		4, 8, # indicies of might-be-unknown args
		defined $disp? 9 : 5 # estimated length
	);
}
sub _append_op8_const_to_mem {
	my ($self, $opcode, $opcode_reg, $immed, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= $regnum64{$base_reg} // croak "$base_reg is not a valid 64-bit register"
		if defined $base_reg;
	$index_reg= $regnum64{$index_reg} // croak "$index_reg is not a valid 64-bit register"
		if defined $index_reg;
	$self->_append_possible_unknown(
		'_encode_op_reg_mem',
		[ 0, $opcode, $opcode_reg, $base_reg, $disp, $index_reg, $scale, 'c', $immed ],
		4, 8, # indicies of might-be-unknown args
		defined $disp? 8 : 4 # estimated length
	);
}

=head2 _append_mathopNN_const

This is so bizarre I don't even know where to start.  Most "math-like" instructions have an opcode
for an immediate the size of the register (except 64-bit which only gets a 32-bit immediate), an
opcode for an 8-bit immediate, and another opcode specifically for the AX register which is a byte
shorter than the normal, which is the only redeeming reason to bother using it.
Also, there is a constant stored in the 3 bits of the unused register in the ModRM byte which acts
as an extension of the opcode.

These 4 methods are the generic implementation for encoding this mess.
Each implementation also handles the possibility that the immediate value is an unknown variable
resolved while the instructions are assembled.

=over

=item C<_append_mathop64_const($opcodeAX32, $opcode8, $opcode32, $opcode_reg, $reg, $immed)>

This one is annoying because it only gets a sign-extended 32-bit value, so you actually only get
31 bits of an immediate value for a 64-bit instruction.

=cut

sub _append_mathop64_const {
	my ($self, @args)= @_; # $opcodeAX32, $opcode8, $opcode32, $opcode_reg, $reg, $immed
	$args[4]= $regnum64{$args[4]} // croak("$args[4] is not a 64-bit register");
	$self->_append_possible_unknown('_encode_mathop64_imm', \@args, 5, 7);
}
sub _encode_mathop64_imm {
	my ($self, $opcodeAX32, $opcode8, $opcode32, $opcode_reg, $reg, $value)= @_;
	use integer;
	my $rex= 0x48 | (($reg & 8)>>3);
	(($value >> 7) == ($value >> 8))?
		pack('CCCc', $rex, $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value)
	: (($value >> 31) == ($value >> 32))? (
		# Ops on AX get encoded as a special instruction
		$reg? pack('CCCV', $rex, $opcode32, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value)
			: pack('CCV', $rex, $opcodeAX32, $value)
	)
	# 64-bit only supports 32-bit sign-extend immediate
	: croak "$value is wider than 32-bit";
}

=item C<_append_mathop32_const($opcodeAX32, $opcode8, $opcode32, $opcode_reg, $reg, $immed)>

=cut

sub _append_mathop32_const {
	my ($self, @args)= @_; # $opcodeAX32, $opcode8, $opcode32, $opcode_reg, $reg, $immed
	$args[4]= $regnum32{$args[4]} // croak("$args[4] is not a 32-bit register");
	$self->_append_possible_unknown('_encode_mathop32_imm', \@args, 5, 7);
}
sub _encode_mathop32_imm {
	my ($self, $opcodeAX32, $opcode8, $opcode32, $opcode_reg, $reg, $value)= @_;
	use integer;
	my $rex= (($reg & 8)>>3);
	(($value >> 7) == ($value >> 8) or ($value >> 8 == 0xFFFFFF))?
		(	$rex? pack('CCCC', 0x40|$rex, $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFF)
				: pack('CCC', $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFF)
		)
		: (($value >> 32) == ($value >> 33))? (
			# Ops on AX get encoded as a special instruction
			$rex? pack('CCCV', 0x40|$rex, $opcode32, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value)
			: $reg? pack('CCV', $opcode32, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value)
			: pack('CV', $opcodeAX32, $value)
		)
		: croak "$value is wider than 32-bit";
}

=item C<_append_mathop16_const($opcodeAX16, $opcode8, $opcode16, $opcode_reg, $reg, $immed)>

=cut

sub _append_mathop16_const {
	my ($self, @args)= @_; # $opcodeAX16, $opcode8, $opcode16, $opcode_reg, $reg, $immed
	$args[4]= $regnum16{$args[4]} // croak("$args[4] is not a 16-bit register");
	$self->_append_possible_unknown('_encode_mathop16_imm', \@args, 5, 8);
}
sub _encode_mathop16_imm {
	my ($self, $opcodeAX16, $opcode8, $opcode16, $opcode_reg, $reg, $value)= @_;
	use integer;
	my $rex= (($reg & 8)>>3);
	(($value >> 7) == ($value >> 8) or ($value >> 8 == 0xFF))?
		(	$rex? pack('CCCCC', 0x66, 0x40|$rex, $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFF)
				: pack('CCCC', 0x66, $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFF)
		)
		: (($value >> 16) == ($value >> 17))? (
			# Ops on AX get encoded as a special instruction
			$rex? pack('CCCCv', 0x66, 0x40|$rex, $opcode16, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFFFF)
			: $reg? pack('CCCv', 0x66, $opcode16, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFFFF)
			: pack('CCv', 0x66, $opcodeAX16, $value)
		)
		: croak "$value is wider than 16-bit";
}

=item C<_append_mathop8_const($opcodeAX8, $opcode8, $opcode_reg, $reg, $immed)>

On the upside, this one only has one bit width, so the length of the instruction is known even if
the immediate value isn't.

However, we also have to handle the case where "dil", "sil", etc need a REX prefix but AH, BH, etc
can't have one.

=back

=cut

sub _append_mathop8_const {
	my ($self, $opcodeAX8, $opcode8, $opcode_reg, $reg, $immed)= @_;
	use integer;
	$reg= $regnum8{$reg};
	my $value= ref $immed? 0x00 : $immed;
	(($value >> 8) == ($value >> 9)) or croak "$value is wider than 8 bits";
	if (!defined $reg) {
		$reg= $regnum8_high{$_[1]} // croak("$reg is not a 8-bit register");
		$self->{_buf} .= pack('CCC', $opcode8, 0xC0 | ($opcode_reg<<3) | ($reg & 7), $value&0xFF);
	} elsif (!$reg) {
		$self->{_buf} .= pack('CC', $opcodeAX8, $value&0xFF);
	} elsif ($reg > 3) {
		$self->{_buf} .= pack('CCCC', 0x40|(($reg & 8)>>3), $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFF);
	} else {
		$self->{_buf} .= pack('CCC', $opcode8, 0xC0 | ($opcode_reg << 3) | ($reg & 7), $value&0xFF);
	}
	$self->_mark_unresolved(-1, encode => '_repack8', value => $immed)
		if ref $immed;
	$self;
}

sub _append_mathop64_const_to_mem {
	my ($self, $opcode8, $opcode32, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= ($regnum64{$base_reg} // croak "$base_reg is not a 64-bit register")
		if defined $base_reg;
	$index_reg= ($regnum64{$index_reg} // croak "$index_reg is not a 64-bit register")
		if defined $index_reg;
	$self->_append_possible_unknown('_encode_mathop64_mem_immed', [ $opcode8, $opcode32, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale ], 3, defined $disp? 9:12);
}
sub _encode_mathop64_mem_immed {
	my ($self, $opcode8, $opcode32, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	use integer;
	(($value >> 7) == ($value >> 8))?
		$self->_encode_op_reg_mem(8, $opcode8, $opcode_reg, $base_reg, $disp, $index_reg, $scale, 'C', $value&0xFF)
	: (($value >> 31) == ($value >> 32))?
		$self->_encode_op_reg_mem(8, $opcode32, $opcode_reg, $base_reg, $disp, $index_reg, $scale, 'V', $value&0xFFFFFFFF)
	: croak "$value is wider than 31-bit";
}

sub _append_mathop32_const_to_mem {
	my ($self, $opcode8, $opcode32, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= ($regnum64{$base_reg} // croak "$base_reg is not a 64-bit register")
		if defined $base_reg;
	$index_reg= ($regnum64{$index_reg} // croak "$index_reg is not a 64-bit register")
		if defined $index_reg;
	$self->_append_possible_unknown('_encode_mathop32_mem_immed', [ $opcode8, $opcode32, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale ], 3, defined $disp? 12:8);
}
sub _encode_mathop32_mem_immed {
	my ($self, $opcode8, $opcode32, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	use integer;
	(($value >> 7) == ($value >> 8) or ($value >> 8 == 0xFFFFFF))?
		$self->_encode_op_reg_mem(0, $opcode8, $opcode_reg, $base_reg, $disp, $index_reg, $scale).pack('C',$value&0xFF)
	: (($value >> 32) == ($value >> 33))?
		$self->_encode_op_reg_mem(0, $opcode32, $opcode_reg, $base_reg, $disp, $index_reg, $scale).pack('V', $value&0xFFFFFFFF)
	: croak "$value is wider than 32-bit";
}

sub _append_mathop16_const_to_mem {
	my ($self, $opcode8, $opcode16, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= ($regnum64{$base_reg} // croak "$base_reg is not a 64-bit register")
		if defined $base_reg;
	$index_reg= ($regnum64{$index_reg} // croak "$index_reg is not a 64-bit register")
		if defined $index_reg;
	$self->{_buf} .= "\x66";
	$self->_append_possible_unknown('_encode_mathop16_mem_immed', [ $opcode8, $opcode16, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale ], 3, defined $disp? 10:6);
}
sub _encode_mathop16_mem_immed {
	my ($self, $opcode8, $opcode16, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	use integer;
	(($value >> 7) == ($value >> 8) or ($value >> 8 == 0xFF))?
		$self->_encode_op_reg_mem(0, $opcode8, $opcode_reg, $base_reg, $disp, $index_reg, $scale).pack('C',$value&0xFF)
	: (($value >> 16) == ($value >> 17))?
		$self->_encode_op_reg_mem(0, $opcode16, $opcode_reg, $base_reg, $disp, $index_reg, $scale).pack('v', $value&0xFFFF)
	: croak "$value is wider than 16-bit";
}

sub _append_mathop8_const_to_mem {
	my ($self, $opcode8, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	$base_reg= ($regnum64{$base_reg} // croak "$base_reg is not a 64-bit register")
		if defined $base_reg;
	$index_reg= ($regnum64{$index_reg} // croak "$index_reg is not a 64-bit register")
		if defined $index_reg;
	$self->_append_possible_unknown('_encode_mathop8_mem_immed', [ $opcode8, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale ], 2, defined $disp? 10:6);
}
sub _encode_mathop8_mem_immed {
	my ($self, $opcode8, $opcode_reg, $value, $base_reg, $disp, $index_reg, $scale)= @_;
	use integer;
	(($value >> 8) == ($value >> 9)) or croak "$value is wider than 8 bit";
	$self->_encode_op_reg_mem(0, $opcode8, $opcode_reg, $base_reg, $disp, $index_reg, $scale).pack('C',$value&0xFF);
}

=head2 C<_encode_jmp_cond>

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

sub _append_possible_unknown {
	my ($self, $encoder, $encoder_args, $unknown_pos, $estimated_length)= @_;
	if (ref $encoder_args->[$unknown_pos]) {
		$self->mark_unresolved(
			$estimated_length,
			encode => sub {
				my $self= shift;
				my @args= @$encoder_args;
				$args[$unknown_pos]= $args[$unknown_pos]->value
					// croak "Value $encoder_args->[$unknown_pos] is still unresolved";
				$self->$encoder(@args);
			},
		);
	}
	else {
		$self->{_buf} .= $self->$encoder(@$encoder_args);
	}
	$self;
}

sub _append_possible_unknown2 {
	my ($self, $encoder, $encoder_args, $unknown1_pos, $unknown2_pos, $estimated_length)= @_;
	if (ref $encoder_args->[$unknown1_pos] || ref $encoder_args->[$unknown2_pos]) {
		$self->mark_unresolved(
			$estimated_length,
			encode => sub {
				my $self= shift;
				my @args= @$encoder_args;
				$args[$unknown1_pos]= $args[$unknown1_pos]->value
					// croak "Value $encoder_args->[$unknown1_pos] is still unresolved";
				$args[$unknown2_pos]= $args[$unknown2_pos]->value
					// croak "Value $encoder_args->[$unknown2_pos] is still unresolved";
				$self->$encoder(@args);
			},
		);
	}
	else {
		$self->{_buf} .= $self->$encoder(@$encoder_args);
	}
	$self;
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

sub _repack32 {
	my ($self, $params)= @_;
	my $v= $params->{value}->value;
	defined $v or croak "Placeholder $params->{value} has not been assigned";
	return pack('V', $v);
}

sub _repack8 {
	my ($self, $params)= @_;
	my $v= $params->{value}->value;
	defined $v or croak "Placeholder $params->{value} has not been assigned";
	(($v >> 8) == ($v >> 9)) or croak "$v is wider than 8 bits";
	return pack('C', $v & 0xFF);
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
