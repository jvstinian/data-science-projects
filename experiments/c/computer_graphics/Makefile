.PHONY: clean

cg: cg.c
	gcc -g -Og -Wall -Wextra cg.c `pkg-config --libs sdl2` -lm -o cg

clean:
	@rm -f *.ali
	@rm -f cg.o cg
