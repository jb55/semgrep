redo-ifchange $1.hs
exec >&2
ghc --make $1.hs -threaded -rtsopts -O2 -o $3
