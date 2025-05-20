# What is this !?
This is a very slow compression tool made in haskell.
No multithreading , nope nothing.
Its work in progress just check the main function in app/Main.
Thats all.

# How do I use it ?
This is currently in testing.
You must create a file that is called test.txt and fill it with your hearts content.
After running it will create both binary and uncompressed file.

# How do I build it
## If you have Nix
This is just optional you can just use cabal to build
But I used this dev flake while I build it. This may help reproducibility
```
nix develop
cabal build
```
## Normal build
Just use this
```
cabal build
```
# Running 
```
cabal run
```
