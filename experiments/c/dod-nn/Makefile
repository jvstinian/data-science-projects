
# Tried a number of options so that I could use a "-g -O0" debug build,
# but ran into conflicts with _FORTIFY_SOURCE, which I believe is beingj
# defined in a predefined header stdc-predef.h.
# I tried using -undef, -ffreestanding, -fno-stack-protector, and -U_FORTIFY_SOURCE but
# I still got warnings when compiling.
# Defining a file my_predef.h with the line "#undef _FORTIFY_SOURCE" and
# passing "-include my_predef.h" to gcc does seem to successfully disable
# _FORTIFY_SOURCE, i.e. using 
# gcc -include my_predef.h -U _FORTIFY_SOURCE -g -O0 main.c -o main
# I decided to stick with "-g -O-g" though.
build:
	gcc main.c -std=c89 -g -Og -Wall -Wextra -o main

build-prod:
	gcc main.c -std=c89 -DNDEBUG -O3 -Wall -Wextra -o main-prod

build-asm:
	gcc -S main.c -std=c89 -DNDEBUG -march=native -O3 -Wall -Wextra -o main.asm

run: build
	./main
