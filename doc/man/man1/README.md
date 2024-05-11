# Manpages

This directory contains manpages for `castle-model-viewer` and `castle-model-converter`.

Source version is maintained in AsciiDoctor format, see the files with `.adoc` extension. The AsciiDoctor format is good for us, since we also use AsciiDoctor to maintain our website ( https://github.com/castle-engine/cge-www ) -- so CGE developers are already familiar with AsciiDoctor and we can even copy-paste some parts from the website into manpages.

Run `make` to generate manpages in the final format (`.1` files). This requires `asciidoctor`.

After generation, view them like `man ./castle-model-viewer.1`.

See about AsciiDoctor manpage backend: https://docs.asciidoctor.org/asciidoctor/latest/manpage-backend/