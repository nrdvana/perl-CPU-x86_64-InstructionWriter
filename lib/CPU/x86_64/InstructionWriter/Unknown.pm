package CPU::x86_64::InstructionWriter::Unknown;

use strict;
use warnings;
use Carp;

# ABSTRACT: Placeholder for a constant that will be assembled

sub new {
	my ($class, %fields)= @_;
	bless \%fields, $class;
}

sub name { $_[0]{name}= $_[1] if @_ > 1; $_[0]{name} }

sub bits {
	my $self= shift;
	return $self->{bits} unless @_;
	my $val= shift;
	!defined $self->{bits} || $self->{bits} == $val
		or croak "Can't change bits from $self->{bits} to $val for unknown($self->{name})";
	$self->{bits}= $val;
}

sub value { $_[0]{value}= $_[1] if @_ > 1; $_[0]{value} }

1;
