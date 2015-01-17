require 'hell'
require 'Builder'
dofile 'builders/c.lua'

print (gcc {input = {'oi.c', 'dois.c'}})

dofile 'builders/copy.lua'

print (copy {input = {'minha', 'vez'}})
