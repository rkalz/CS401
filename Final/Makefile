UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin)
	CXX := clang++
	CXXFLAGS := -std=c++11
else
	CXX := g++
	CXXFLAGS := -std=c++11 -lpthread
endif

all: build run clean

build:
	$(CXX) $(CFLAGS) CS401Final.cpp -o CS401Final

clean:
	rm CS401Final

run:
	./CS401Final
