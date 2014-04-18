CSC      = csc
CSCFLAGS = -scrutinize -C -Wno-int-to-pointer-cast -prologue prologue.scm
LD       = csc
LDFLAGS  =

eggs = utf8 lalr

target = lambda
objects = interpreter.o lambda-ast.o lambda-main.o lambda-operations.o \
	  lambda-parser.o lambda-sugar.o lambda-target.o lambdaNB-parser.o \
	  scheme-target.o simply-typed-parser.o target.o

clean = $(objects) $(target)

all: $(target)

include rules.mk

$(target): $(objects)
	$(call cmd,ld)

lambda.yy.scm: lambda-grammar.scm
	@echo "  GEN     $@"
	@csi < $^ > /dev/null

lambdaNB.yy.scm: lambdaNB-grammar.scm
	@echo "  GEN     $@"
	@csi < $^ > /dev/null

simply-typed.yy.scm: simply-typed-grammar.scm
	@echo "  GEN     $@"
	@csi < $^ > /dev/null

lambda-parser.o: lambda-lexer.scm lambda.yy.scm
lambdaNB-parser.o: lambda-lexer.scm lambdaNB.yy.scm
simply-typed-parser.o: lambda-lexer.scm simply-typed.yy.scm

lambda-operations.o: ast-case.scm
lambda-target.o: ast-case.scm
scheme-target.o: ast-case.scm

eggs:
	chicken-install $(eggs)
