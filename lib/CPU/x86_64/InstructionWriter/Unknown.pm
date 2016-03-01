package CPU::x86_64::InstructionWriter::Unknown;
use strict;
use warnings;
use Carp;
use Scalar::Util 'refaddr';

sub new {
	my ($class, %fields)= @_;
	bless \%fields, $class;
}

sub name {
	my $self= shift;
	return $self->{name} // "$self"
		unless @_;
	$self->{name}= shift;
}

sub bits {
	my $self= shift;
	return $self->{bits} unless @_;
	my $val= shift;
	!defined $self->{bits} || $self->{bits} == $val or croak "Can't change bits from $self->{bits} to $val for unknown($self->{name})";
	$self->{bits}= $val;
	$self->try_resolve
	$self->{bits};
}

sub value {
	my $self= shift;
	return $self->{value} unless @_;
	my $val= shift;
	!defined $self->{value} || $self->{value} == $val or croak "Can't change value of unknown($self->{name})";
	$self->{value}= $val;
	$self->try_resolve;
	$self->{value};
}

our %deps;
sub _add_dep {
	my ($writer, $chain, $unknown, $address, $instruction, $current_size)= @_;
	push @{ $deps{refaddr $writer} }
}

1;
