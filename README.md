# 2048 Game in Haskell

This project is an implementation of the popular 2048 game using Haskell. The game is played on a 4x4 grid with numbered tiles that slide when a player moves them. The objective is to combine tiles with the same number to create a tile with the number 2048.

## Features

- Move tiles up, down, left, or right using the WASD keys.
- Combine tiles with the same number to create larger numbers.
- Randomly adds a new tile (2 or 4) to an empty spot after each move.
- Detects when the game is won or lost.

## How to Play

1. Compile the program using GHC or run it directly if you have a Haskell environment set up.
2. Use the WASD keys to move the tiles:
   - `W` to move up
   - `A` to move left
   - `S` to move down
   - `D` to move right
3. The goal is to reach the tile with the number 2048.
4. If no moves are possible and there are no empty spots, the game is over.

## Requirements

- GHC (The Glasgow Haskell Compiler)
- The following Haskell libraries:
  - `System.IO`
  - `Data.List`
  - `System.Random`

## Installation and Running

### Installation

1. Install GHC from the [official website](https://www.haskell.org/ghc/).
2. Save the `main.hs` file to your local machine.

### Running

1. Open a terminal.
2. Navigate to the directory where `main.hs` is saved.
3. Compile the file using GHC:

   ```bash
   ghc main.hs -o 2048
