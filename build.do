redo-ifchange install
redo-ifchange $(./deps) compiler configure
./compiler build --ghc-options=-Wall
