package CPU::x86_64::InstructionWriter::Label;

use strict;
use warnings;
use Carp;

sub relative_to { @_ > 1 && carp "Read-only"; $_[0]{relative_to} }
sub name  { @_ > 1 && carp "Read-only"; $_[0]{name} }
sub start { @_ > 1 && carp "Read-only"; $_[0]{start} }
sub value {
	my $start= $_[0]{start};
	my $rel= !defined $_[0]{relative_to}? 0
		: ref $_[0]{relative_to}? $_[0]{relative_to}->value
		: $_[0]{relative_to};
	defined $start && defined $rel? $start + $rel : undef;
}

1;
