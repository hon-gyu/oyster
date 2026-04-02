#!/bin/bash
set -e

echo "Installing OCaml dependencies..."
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    curl \
    git \
    unzip \
    bubblewrap \
    m4 \
    opam \
    ripgrep \
    pkg-config \
    libgmp-dev

echo "Initializing opam..."
opam init -c 5.4.0 --disable-sandboxing -y
eval $(opam env)

echo "Installing OCaml LSP server and common tools..."
opam install -y \
    ocaml-lsp-server \
    dune \
    merlin \
    ocamlformat \
    odoc \
    utop

opam clean -a

echo "Setting up shell environment..."
echo 'eval $(opam env)' >> ~/.bashrc
echo 'eval $(opam env)' >> ~/.zshrc

echo "OCaml setup complete!"
opam --version
ocaml --version
which ocamllsp
