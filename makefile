## Hell Build System Makefile ##

# The source files
lua_src = src/lua/*.lua
cpp_dir = src/cpp
cpp_src = $(cpp_dir)/*.cpp
builders = src/builders

export BUILD := $(CURDIR)/build

# default target: make all
all : lua cpp

# build lua stuff (just copy =P)
lua : $(lua_src)
	cp $(lua_src) build
	cp -R $(builders) build

# call make for cpp stuff
cpp :
	$(MAKE) -C $(cpp_dir)

.PHONY : clean
clean :
	$(MAKE) -C $(cpp_dir) clean
	$(RM) -r *~ build/*
