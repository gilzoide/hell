-- como eu imagino que os módulos de regras de construção (builders) devem ser

-- lembrando que builders são rodados direto do hell.lua, então não precisamos
-- executar `require 'Builder'`. Funções auxiliares pra construção de Builders
-- podem ser encontradas no 'hellutils.lua', esse pode requerir se precisar

require 'hellutils'


--[[		Builder é uma metatable com alguns campos padrão, a serem usados pra se montar os comandos a serem executados		]]--
-- campos com '*' são considerados padrão para builders, q geralmente serão usados
c = Builder {
	bin = 'gcc',	-- * binário usado pra construção da saída desejada (bacana deixar separado pra poder ser sobreposto)
	flags = 'Wall g O2',	-- * flags pro `bin'
	ext = 'o',	-- * extensão de saída, maioria das vezes é padrão
	out = input,	-- nome da saída, em geral só troca extensão a partir do input
	links,	-- no caso do C, podemos usar um campo de linkagem, que pega info a partir do pkg-config
	opts,	-- * opções do builder, teremos alguns predefinidos, como `Recursivo', `PreservaEstruturaDoDiretório' (é, n pensei mto mais q isso...)
	cmd = '$bin $flags $input -o $out $links'	-- *** NECESSÁRIO: comando a ser executado, com substituição de variável por valores de campos da table (qualquer `nil' vira "", não se preocupe)
}

-- Builders podem ter Builders dentro de si, sendo o de fora o nome da extensão de entrada padrão (q a função `build' descobre qual Builder chamar sem especificação por causa disso)
--	Usando um pouco da abstração de Hierarquia, Builders internos herdam todos os campos dos seus pais
--		os novos valores dos campos podem ser complementares aos iniciais (só avisar, com um '&' no começo), sendo assim concatenados
--		por padrão, ele substitui. Pode-se forçar substituição com '!' (escape pro '&')
c.sharedLib = Builder {
	flags = '&c',	-- esse complementa flags
	picFlags = flags .. 'fPIC',
	soFlags = flags .. 'shared',
	ext = 'so',
	cmd = '$bin $picFlags $input -o $picOut $links && $bin -c $soFlags $input -o $soOut $links'
}
-- e podem também ser extendidos a qualquer hora, que daí não precisa o por dentro de um outro builder
c14 = c:extend {
	flags = '&std=C14'
}

verb_cp = copy:extend {
	flags = '&v'
}

--[[		A imaginação é o limite aqui, pq podemos criar Builders pra qualquer coisa!
			Pode-se fazer um builder pra manpage, zippar coisas, alguma extensão nova sua!		]]--
myExt = Builder {
	cmd = 'echo "minha extensão é construída com sucesso, e posso mandar esse builder pros meus amigos usuários!"'
}


--[[		Preparando os campos
			Pode-se fazer o builder preparar algum campo antes de esse ser usado, pondo prefixo, sufixo, mudando extensão, você tem o poder!		]]--
-- Para preparar um campo, use uma função que receba o campo e, opcionalmente, o próprio builder
--	essa função deve ter como nome 'prepare_CAMPO', sendo CAMPO o campo a ser preparado.
--	Várias funções auxiliares estarão disponíveis no arquivo 'hellutils.lua', cola lá que explica bem (en)
c.linka = Builder {
	-- ao por um valor no campo 'link', ele automagicamente põe o '-l' antes =]
	-- as funções auxiliares podem ajudar bastante, lembre-se delas!
	prepare_links = curryPrefixEach ('-l')
}
			


--[[		Builders serão ligados dinamicamente ao buildSystem, então podem ser baixados diretamente e colocado script na pasta certa, e voilá!		]]--
