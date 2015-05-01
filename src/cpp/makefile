## Hell C++ stuff Makefile ##

objs = cppUtils.o processBI.o build.o
lib_obj = cppUtils.so

# Lua include, as seen on different Linux distros
lualib = $(shell if pkg-config --list-all | grep lua5.2; then echo "lua5.2"; else echo "lua"; fi)
LINKS = $(shell pkg-config --cflags --libs $(lualib))

CXX = g++
CXXFLAGS = -Wall -g -O2 -std=c++11 -fPIC $(LINKS)


all : $(objs)
	$(CXX) -o $(BUILD)/$(lib_obj) $^ -shared $(CXXFLAGS) -lc

#PIC : $(lib_src)
%.o : %.cpp
	$(CXX) -o $@ -c $< $(CXXFLAGS)

.PHONY : clean
clean :
	$(RM) *~ *.o

