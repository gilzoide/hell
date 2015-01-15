-- como eu imagino que o script deva ser

--[[		Uma função build, que constrói qualquer coisa!			]]--
-- baseado na extensão
build { src = 'oi_mundo.c' }
build { src = 'eita_sô.java' }
-- como construtor padrão, pra extensões/arquivos desconhecidos, simplesmente copia (construtor `copy' (cópia), oi c++ =P)
build { src = 'readme.txt', out = 'copia.txt' }
-- talvez fazer até um recursivo, usando o DirTreeIterator (http://lua-users.org/wiki/DirTreeIterator) ou outro paranauê
build { src = 'src_dir_for_a_java_pkg', opts = recurseOver '*.java' + preserveDirStructure }
-- e, claro, um em q vc escolhe o construtor (regra a ser seguida pra construção) ;]
build { src = 'oi.png doido.png minha_nossa.png', builder = convertPngToJpg }
-- talvez chamar a função build pelo construtor, pra n ter q por mais uma variável na table (pura frescura, mas pode ser bacana)
--	ideia: usar um método
cpp.sharedLib:build { src = '*.cpp' }
-- quaisquer adições ao construtor podem ser feitas a qualquer hora (caham, flags pra compiladores, links, talvez até otro binário pra executar a build!)
build { src = 'mais_um.cc', flags = 'std=c++11 fpermissive', links = 'lua5.2', bin = 'llvm' }
-- e, claro, temos que poder declarar dependências, né
--	e essas talvez devam poder incluir outras builds!
build { src = 'construa-me.c', deps = 'construa-me.h' }
build { src = 'construa-me.c', deps = build { src = 'dependencia.c', builder = c.staticLib } }
num1 = build { src = 'blablabla' }
build { src = 'construa-me.c', deps = { num1, 'outra.c' } }

--[[		Install: porque instalar software, mtas das vezes, é o que a gente quer!
			installs são os targets pro comando `hell install` e `hell uninstall`, pra facilitar a vida		]]--
-- install da build
install { num1, out = '/usr/bin' }
-- ou igual escreve a build mesmo, mas com a saída bonita, né
install { src = 'construa-me.c', out = '$prefix/lib' }


--[[		Targets: estilo make, bom pra fazer builds diferentes, tipo de debug, ou sei lá		]]--
-- é só por a build dentro de uma table, e voilá!
--	como a table chama 'yeah', pra construir esse target é só mandar o comando `hell yeah`
--	isso também rola pra installs, no caso acima pode mandar um `hell yeah install`
yeah = {	
	build { src = 'putz', out = 'copiou'},
	install { src = 'grila', out = 'copiou_tambem'}
}
-- tables dentro de tables? não é problema!
--	`hell yeah.man`
yeah = {
	man = {
		build { src = 'tanto_faz' }
	}
}


--[[		Recursivo, permite a escalabilidade (que é um aspecto importante de build systems)!		]]--
-- é só chamar o próximo script =P
addHellBuild ('src/hellbuild.lua')
feedHellFire ('src/hellfire.lua')
-- pode por dentro de target
yeah = {
	feedHellFire ('src/module/hellfire.lua')
}


--[[		Escolha de vários parâmetros gerais, como arquio de saída, verbose, silent; globais ou por build		]]--
-- como visto ali acima, `out' é a saída
build { src = 'readme.txt', out = 'copia.txt' }
-- mas devemos poder alterar o caminho padrão de contrução (shadow build), com padrão o diretório '.'
--	que daí, ao usar o `out' ou não, ele põe relativo à pasta em questão, que pode ser criada se não existir
-- TODO achar um jeito de ter esse outDir não só global mas n completamente local, talvez usar `environments', q nem o scons (que não acho mto bacana, q complica o rolê)
hell.outDir = 'build'
-- silent/verbose, sendo que flags pro programa sobrepõem esse padrão aqui da linha de baixo
hell.silent = true
hell.verbose = true
build { src = 'putz_grila.lua', silent = true }
-- quem sabe até cor da saída (pqp, pra q?) =P
hell.color = RED
build { src = 'oi', color = BLUE }

--[[		Uma meta é podermos criar regras facilmente, inclusive "on the fly"
			pra isso, leia o `moduleScript.lua' que está neste diretório ;]			]]--
