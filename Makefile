.PHONY: compile
compile:
	./compile.sh

.PHONY: install
install:
	$(MAKE) -C desktop/ install

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	castle-engine clean
	rm -f tovrmlx3d tovrmlx3d.exe
	rm -Rf view3dscene.app tovrmlx3d.app macosx/view3dscene.app macosx/*.dmg
