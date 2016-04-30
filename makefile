## Hell Build System Makefile ##

prefix = /usr
lib_install_path = $(prefix)/lib/hell
bin_install_path = $(prefix)/bin

# The source files
lua_src = src/lua/*.lua
cpp_dir = src/cpp
cpp_src = $(cpp_dir)/*.cpp
builders = src/builders

# permission modes
permissions = 755

export BUILD := $(CURDIR)/build

# default target: make all
all : builddir lua cpp

# build lua stuff (just copy =P)
lua : $(lua_src)
	cp $(lua_src) build
	cp -R $(builders) build

# call make for cpp stuff
cpp :
	$(MAKE) -C $(cpp_dir)

builddir :
	@mkdir -p $(BUILD)


# INSTALL #
install :
	install -d $(lib_install_path) $(lib_install_path)/builders $(bin_install_path)
	install -m $(permissions) build/*.lua $(lib_install_path)
	install -m $(permissions) build/*.so $(lib_install_path)
	install -m $(permissions) build/builders/*.lua $(lib_install_path)/builders
	install -m $(permissions) build/hell.lua $(bin_install_path)/hell


.PHONY : clean
clean :
	$(MAKE) -C $(cpp_dir) clean
	$(RM) -r *~ build/*
