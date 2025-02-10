# Scripts and helpers for Snap packages

TODO: The Snap packaging of Castle Model Viewer is in-progress.

We plan to release castle-model-viewer and castle-model-converter, 2 separate Snap packages. The packages will be available on the Snap Store.

TODO:
- read https://snapcraft.io/docs/desktop-applications .
- how to make a desktop file , register MIME types outside of snap -- see what our install.sh does inside the snap.
- read https://snapcraft.io/docs/gpu-support

## Building and testing

Do this once (more information: [Snapcraft overview](https://snapcraft.io/docs/snapcraft-overview)):

```bash
sudo snap install snapcraft --classic

# If you get question like this:
#   LXD is required but not installed. Do you wish to install LXD and configure it with the defaults? [y/N]: y
# -> answer "y".

# If you get error like this:
#     craft-providers error: Failed to install LXD: user must be manually added to 'lxd' group before using LXD.
#     Visit https://documentation.ubuntu.com/lxd/en/latest/getting_started/ for instructions on installing and configuring LXD for your operating system.
# -> do this, relogin and retry:
ME=`id --user --name`
sudo adduser "${ME}" lxd
```

To build the Snap package:

```bash
./make_snap.sh
```

If something is wrong:
- The `snapcraft --debug` (executed by `make_snap.sh`) will enter a shell inside the environment.
- You can edit the `snap/snapcraft.yaml` file, and then run `snapcraft` inside the shell to rebuild the Snap package.
- Don't forget to synchronize changes to the `snapcraft.yaml.template`, check `diff -u snapcraft.yaml.template snap/snapcraft.yaml`.

To test the Snap package:

```bash
sudo snap install --devmode castle-model-viewer_*_amd64.snap
snap run castle-model-viewer
sudo snap remove castle-model-viewer
```
