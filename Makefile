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
	rm -f view3dscene view3dscene.exe code/view3dscene code/view3dscene.exe \
	      tovrmlx3d tovrmlx3d.exe code/tovrmlx3d code/tovrmlx3d.exe
	rm -Rf view3dscene.app tovrmlx3d.app macosx/view3dscene.app macosx/tovrmlx3d.app macosx/*.dmg
	$(MAKE) -C images/ clean
	$(MAKE) -C internal_scenes/ clean
	$(MAKE) -C screen_effects/ clean
