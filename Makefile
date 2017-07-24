.RECIPEPREFIX+=

.PHONY: clean

clean:
  rm -f target/hello*

# target/hello.s:
#   mkdir -p target
#   gcc -S hello.c -o target/hello.s

#target/hello.s
target/hello.o:
  as hello.s -o target/hello.o

target/hello: target/hello.o
  ld target/hello.o -o target/hello
