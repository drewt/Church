CSC      = csc
CSCFLAGS = -scrutinize -C -Wno-int-to-pointer-cast -prologue prologue.scm
LD       = csc
LDFLAGS  =

eggs = utf8 lalr

target = lambda
objects = lambda-ast.o lambda-interpreter.o lambda-parser.o lambda-target.o \
	  target.o

clean = $(objects) $(target)

all: $(target)

include rules.mk

$(target): $(objects)
	$(call cmd,ld)

lambda.yy.scm: lambda-grammar.scm
	@echo "  GEN     $@"
	@csi < $^ > /dev/null

lambda-parser.o: lambda-lexer.scm lambda.yy.scm

eggs:
	chicken-install $(eggs)
