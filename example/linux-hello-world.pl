use v5.36;
use ELF::Writer::Linux_x86_64;
use CPU::x86_64::InstructionWriter;

my $program= CPU::x86_64::InstructionWriter->new
   # write(1, "Hello World\n", 12)
   ->mov('rax', 1)->mov('rdi', 1)->lea('rsi', [rip => \"hello world"])->mov('rdx', 12)->syscall
   # exit(0)
   ->mov('rax', 60)->mov('rdi', 0)->syscall
   ->label("hello world")->data("Hello World\n")
   ->bytes;

my $elf= ELF::Writer::Linux_x86_64->new(type => 'executable');
my $prog_ofs= $elf->elf_header_len + $elf->segment_header_elem_len;
my $seg_addr= 0x10000;
push @{$elf->segments}, ELF::Writer::Segment->new({
   offset => 0,
   virt_addr => $seg_addr,
   data_start => $prog_ofs,
   data => $program,
});
$elf->entry_point($seg_addr + $prog_ofs);
$elf->write_file("hello");
exec "./hello";