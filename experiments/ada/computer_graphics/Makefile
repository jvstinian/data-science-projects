.PHONY: clean

build:
	gnatmake cg.adb -cargs `pkg-config --cflags sdl2` -largs `pkg-config --libs sdl2`

clean:
	@rm -f *.ali
	@rm -f cg.o cg
