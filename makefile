
asm:
	nasm -felf32 -g test.s
	ld -melf_i386 -e tiny -g test.o -o test.app
	./test.app

%.png: %.dot
	dot $< -Tpng -o $@
