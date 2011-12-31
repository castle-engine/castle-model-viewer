.PHONY: compile
compile:
	./compile.sh

.PHONY: install
install:
	$(MAKE) -C desktop/ install

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	rm -f view3dscene view3dscene.exe tovrmlx3d tovrmlx3d.exe
