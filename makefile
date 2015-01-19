src = src/*.lua
builders = src/builders

copy : $(src)
	cp $(src) build
	cp -R $(builders) build

clean :
	$(RM) build/*
