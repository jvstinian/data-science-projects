.PHONY: build

.DEFAULT_GOAL := build

poisson.o: poisson.c
	gcc poisson.c -c -std=c89 -g -Og -Wall -Wextra -o poisson.o

carrental.o: carrental.c
	gcc carrental.c -c -std=c89 -g -Og -Wall -Wextra -o carrental.o

blackjack.o: blackjack.c
	gcc blackjack.c -c -std=c89 -g -Og -Wall -Wextra -o blackjack.o

ttt.o: ttt.c
	gcc ttt.c -c -std=c89 -g -Og -Wall -Wextra -o ttt.o

build: poisson.o carrental.o blackjack.o ttt.o
	gcc carrental.o poisson.o blackjack.o -lm -o main

run: build
	./main
