package TestASM;
use strict;
use warnings;
use Exporter 'import';
use IO::Handle;
use File::Temp;
use File::Spec::Functions 'splitpath', 'splitdir', 'catdir', 'catpath';
use Digest::MD5 'md5_hex';

our @EXPORT= qw( reference_assemble hex_diff have_nasm );

sub which_nasm {
	return $ENV{NASM_PATH} if defined $ENV{NASM_PATH};
	chomp(my $path= `which nasm`);
	$? == 0 or die "Can't locate nasm (tests require nasm unless cached in t/nasm_cache)\nInstall NASM or set environment var \$NASM_PATH\n";
	$path;
}

sub nasm_cache_file {
	my $md5= shift;
	my ($vol, $path, $file)= splitpath(__FILE__);
	$path =~ s,[\\/]$,,; # IMO this is a bug in File::Spec
	my @dirs= splitdir($path);
	$dirs[-1]= 'nasm_cache';
	catpath($vol, catdir(@dirs), $md5);
}

sub reference_assemble {
	my $asm_source= shift;
	my $md5= md5_hex($asm_source);
	my $cache_file= nasm_cache_file($md5);
	unless (-f $cache_file) {
		# Can't pipe to nasm, because it tries to mmap the input, or something.
		# So use clumsy temp files.
		my $infile= File::Temp->new(TEMPLATE => 'asm-XXXXXX', SUFFIX => '.asm')
			or die "tmpfile: $!";
		$infile->print("[bits 64]\n".$asm_source);
		$infile->close;
		mkdir nasm_cache_file(undef);
		unless (system(which_nasm, '-o', $cache_file, $infile) == 0) {
			my $e= $?;
			system('cp', $infile, 'died.asm');
			die "nasm: $e (input = died.asm)";
		}
	}
	open my $fh, '<:raw', $cache_file or die "open: $!";
	local $/= undef;
	scalar <$fh>;
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
