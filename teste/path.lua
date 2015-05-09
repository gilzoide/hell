local util = hell.utils
base = '../build/oi/'
path = '../../../src'
print (util.makeRelative (path, base))
print (util.makeRelative ('../../', ''))
print (util.makeRelative ('./../', ''))
