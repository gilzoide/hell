## Hell C++ stuff Makefile ##

objs = cppUtils.o commonLib.o Build.o BuildGraph.o CycleLogger.o JobManager.o
lib_obj = cppUtils.so

# Lua include, as seen on different Linux distros
lualib = $(shell if pkg-config --list-all | grep lua5.3 > /dev/null; then echo "lua5.3"; else echo "lua"; fi)

CXX ?= g++
CXXFLAGS = -Wall -g -O2 -std=c++11
# Luarocks Lualib flag, or check with pkg-config
LIBFLAG ?= $(shell pkg-config --libs $(lualib))
LINKS = -pthread $(LIBFLAG)


all : $(objs)
	$(CXX) -o $(BUILD)/$(lib_obj) $^ -shared $(CXXFLAGS) $(LINKS)

%.o : %.cpp %.hpp
	$(CXX) -o $@ -c $< $(CXXFLAGS) -fPIC

.PHONY : clean
clean :
	$(RM) *~ *.o

