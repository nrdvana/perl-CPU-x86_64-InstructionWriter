package TestASM;
use strict;
use warnings;
use Exporter 'import';
use IO::Handle;
use File::Temp;

our @EXPORT= qw( reference_assemble hex_diff have_nasm );

sub have_nasm {
	`which nasm`;
	return $?==0;
}

sub reference_assemble {
	# Can't pipe to nasm, because it tries to mmap the input, or something.
	# So use clumsy temp files.
	my $infile= File::Temp->new(TEMPLATE => 'asm-mov-XXXXXX', SUFFIX => '.asm')
		or die "tmpfile: $!";
	$infile->print("[bits 64]\n".$_[0]);
	$infile->flush;
	my $outfile= File::Temp->new(TEMPLATE => 'asm-mov-XXXXXX', SUFFIX => '.o')
		or die "tmpfile: $!";
	unless (system('nasm', '-o', $outfile, $infile) == 0) {
		my $e= $?;
		system('cp', $infile, 'died.asm');
		die "nasm: $e (input = died.asm)";
	}
	binmode $outfile;
	$outfile->seek(0,0);
	local $/= undef;
	scalar <$outfile>;
}

sub hex_diff {
	my ($data1, $data2)= @_;
	my $o= 0;
	my $ret= '';
	while ($o < length($data1) || $o < length($data2)) {
		my $d1= length $data1 >= $o? substr($data1, $o, 16) : '';
		my $d2= length $data2 >= $o? substr($data2, $o, 16) : '';
		$d1 =~ s/(.)/sprintf("%02x ",ord($1))/gse;
		substr($d1, 24, 0)= ' ' if length $d1 > 24;
		$d2 =~ s/(.)/sprintf("%02x ",ord($1))/gse;
		substr($d2, 24, 0)= ' ' if length $d2 > 24;
		$ret .= sprintf("%-48s |  %-48s%s\n", $d1, $d2, $d1 eq $d2? '':' *');
		$o+= 16;
	}
	return $ret;
}

1;
