--[[		hell: a table que sabe tudo o que tá rolando		]]--
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
}

-- deixa ninguém mexer em campos que não existem
hell.__newindex = function (t, k, v)
	error ("If you praise for your life, don't mess with HELL (the Table)!")
end
