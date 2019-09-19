PROGRAMS := game creator

.PHONY: all
all: $(PROGRAMS)

.PHONY: game
game:
	idris --build game.ipkg

.PHONY: creator
creator:
	idris --build creator.ipkg

.PHONY: clean
clean:
	idris --clean game.ipkg creator.ipkg
