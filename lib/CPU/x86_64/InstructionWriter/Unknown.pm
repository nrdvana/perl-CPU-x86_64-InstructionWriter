package CPU::x86_64::InstructionWriter::Unknown;

use Moo 2;
use Carp;
use Scalar::Util 'refaddr';

# ABSTRACT: Placeholder for a constant that will be assembled

has name => ( is => 'rw' );

sub bits {
	my $self= shift;
	return $self->{bits} unless @_;
	my $val= shift;
	!defined $self->{bits} || $self->{bits} == $val
		or croak "Can't change bits from $self->{bits} to $val for unknown($self->{name})";
	$self->{bits}= $val;
}

has value => ( is => 'rw' );

1;
