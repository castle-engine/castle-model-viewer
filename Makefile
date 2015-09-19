.PHONY: compile
compile:
	./compile.sh

# Run a couple of child targets to autogenerate some code
.PHONY: generate-code
generate-code:
	$(MAKE) -C images/
	$(MAKE) -C internal_scenes/
	$(MAKE) -C screen_effects/

.PHONY: install
install:
	$(MAKE) -C desktop/ install

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	castle-engine clean
	rm -f tovrmlx3d tovrmlx3d.exe
	rm -Rf view3dscene.app tovrmlx3d.app macosx/view3dscene.app macosx/*.dmg
