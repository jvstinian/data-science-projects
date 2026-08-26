.PHONY: clean

cg: cg.c
	gcc -g -Og -Wall -Wextra cg.c `pkg-config --libs sdl2` -o cg

clean:
	@rm -f *.ali
	@rm -f cg.o cg
