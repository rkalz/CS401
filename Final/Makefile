UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin)
	CC := clang++
	CFLAGS := -std=c++11
else
	CC := g++
	CFLAGS := -std=c++11 -lpthread
endif

all: build run clean

build:
	$(CC) $(CFLAGS) CS401Final.cpp -o CS401Final

clean:
	rm CS401Final

run:
	./CS401Final