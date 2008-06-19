.PHONY: compile
compile:
	./compile.sh

.PHONY: install
install:
	$(MAKE) -C desktop/ install
