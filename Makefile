PROGRAMS := game

.PHONY: all
all: $(PROGRAMS)

.PHONY: game
game:
	idris --build game.ipkg

.PHONY: clean
clean:
	rm -f *.ibc $(PROGRAMS)
