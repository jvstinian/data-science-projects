.PHONY: build build-prod run

build:
	gcc frozenlake.c -std=c89 -g -Og -Wall -Wextra -Wswitch -Wswitch-enum -o main

build-prod:
	gcc frozenlake.c -std=c89 -DNDEBUG -O3 -Wall -Wextra -o main-prod

run: build
	./main
