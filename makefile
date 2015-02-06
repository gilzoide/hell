lua_src = src/*.lua
hs_src = src/*.hs
builders = src/builders

all : lua haskell

lua : $(lua_src)
	cp $(lua_src) build
	cp -R $(builders) build

haskell : $(hs_src)
	ghc --make -dynamic $< -o build/main -outputdir build

clean :
	$(RM) build/*
