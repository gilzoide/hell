--[[		hell: a table que sabe tudo o que tá rolando		]]--

local win = {
	name = 'windows',
	architecture = os.getenv ('PROCESSOR_ARCHITECTURE'),
	flag = '/',
	long = '/',
	dir_sep = '\\'
}

local unix = {
	name = 'unix',
	architecture = io.popen ('uname -m'):read (),
	flag = '-',
	long = '--',
	dir_sep = '/'
}

hell = {
	-- diretório de saída: usado pra "shadow builds"
	-- se '', constrói no próprio diretório
	outdir = '',
	-- mantém ou não o formato do diretório da build?
	-- Ex: se entrada tá na pasta 'src', saída vai pra pasta '$outdir/src'
	keepDirStructure = false,
	-- table que contém as builds. Por favor não mexa nela xP
	builds = {},
	-- table que contém as installs. Por favor não mexa nela xP
	installs = {},
	-- ao conferir arquivos de entrada com o glob, usar as entradas em 
	-- um comando só?
	multinput = true,
	-- table com alguns específicos do sistema operacional
	os = (package.config:sub (1, 1) == '/' and unix) or win,
	-- deixa ninguém mexer em campos que não existem
	__newindex = function ()
		error ("If you praise for your life, don't mess with HELL (the Table)!")
	end
}

-- pro __newindex funfar (que metamétodos só servem pra metatables)
setmetatable (hell, hell)
