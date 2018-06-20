package CPU::x86_64::InstructionWriter::Label;

use strict;
use warnings;
use Carp;

sub name  { @_ > 1 && carp "Read-only"; $_[0]{name} }
sub start { @_ > 1 && carp "Read-only"; $_[0]{start} }
*value= *start;

1;
