# Project Goals
The goal of this project is to build a game using the `brick` toolkit. We aim to demonstrate how to use Haskell to develop an application that needs to keep track of state. Our project also aims to inspire players of the game with colorful and creative visual design.

# Raccoon Rush

*Raccoon Rush* is a single-player, Sokoban-style puzzle game. The objective of the game is to move key items (pieces of trash) from their starting positions (a trash can) in a grid to designated final positions (stashes) by pushing it into successive adjacent grid cells. Each level offers different puzzle mechanics and/or a larger grid space.

Our levels are themed after areas on the UCSD campus.

# Game Mechanics

In our game, the player controls a raccoon which occupies one grid cell and is able to move either up, right, down, or left, one grid space at a time. Each level contains at least one piece of trash placed within a grid cell and another grid cell marked as a stash. The objective is to push the trash into the stash grid square to clear the level. The player can push an object to an adjacent cell by moving into the object from the opposite side. Players cannot pull objects, move them diagonally, or move more than one square at a time. Some grid spaces contain walls which are spaces the player and other objects cannot occupy or move through. 

A player also has the ability to reset the state of a level entirely.

# Installation
To run the game, run the command `stack run` in a terminal in the `cse230-project` directory.
To run the tests, run the command `stack test` in a terminal in the `cse230-project` directory.
