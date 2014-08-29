f = open("foo.c","w")
for i in xrange(10000000):
    f.write("int x" + str(i) + ";\n")
