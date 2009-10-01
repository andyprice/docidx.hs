HASKELLS	:=	$(shell find . -name "*.hs")

INCLUDES	=	-isrc -i$(HOME)/work/code/haskell/fsmActions

GHC		=	ghc $(INCLUDES) -Wall

GHCI		=	ghci $(INCLUDES)

RUNGHC		=	runghc $(INCLUDES)

all:			clean configure build haddock sdist

clean:
			@runghc Setup clean && \
			  find . -name '*~' -exec rm -vf {} ';' && \
			  find . -name '*.hi' -exec rm -vf {} ';' && \
			  find . -name '*.o' -exec rm -vf {} ';' && \
			  rm -vf docidx

nuke:			clean

lint:
			hlint $(HASKELLS)

configure:
			$(RUNGHC) Setup configure --user

build:			configure
			$(RUNGHC) Setup build

haddock:		configure
			$(RUNGHC) Setup haddock --executables --hyperlink-source

sdist:
			runghc Setup sdist

view:			haddock
			open dist/doc/html/docidx/docidx/index.html

.PHONY: all clean configure build haddock sdist view lint nuke
