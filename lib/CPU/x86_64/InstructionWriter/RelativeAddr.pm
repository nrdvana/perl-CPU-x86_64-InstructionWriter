package CPU::x86_64::InstructionWriter::RelativeAddr;

use Carp;

# ABSTRACT: Placeholder for a constant, calculated from address to label

sub bits { 31 }

sub target { @_ > 1 && croak "Read-only"; shift->{target} }
sub source { @_ > 1 && croak "Read-only"; shift->{source} }

sub value {
	@_ > 1 && croak "Read-only";
	my $self= shift;
	defined (my $target= $self->{target}) or croak "target not defined on RelativeAddr $self->{name}";
	defined (my $source= $self->{source}) or croak "source not defined on RelativeAddr $self->{name}";
	# TODO: if target and source are both labels on the same instruction stream,
	# then can calculate the offset even if those haven't been resolved.
	$target= $target->value if ref $target;
	$source= $source->value if ref $source;
	return undef
		unless defined $target && defined $source;
	return \($target - $source);
}

1;
