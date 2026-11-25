# Alien Assault

A small Haskell game

![Gameplay](docs/gameplay.png)

# Quick build

- Install ncurses, ghc and cabal (on Arch and NixOS the package is cabal-install)
- Update cabal:

		cabal update


- Build the project:

		cabal build

- Run:

		cabal run

# Gameplay

WASD - Move
SPACE - Fire bullets

Bullets come back down eventually and disappear if they hit someone


# Limitations

Some limitations that will be addressed

- Key input is slow if you hold a key
- This is in the terminal so make sure the window is large enough
