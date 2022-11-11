# minesweeper-haskell

We will be developing the popular Microsoft game Minesweeper. Minesweeper is a logic puzzle video game that features a grid of clickable squares, with hidden "mines" scattered throughout the board. We will be using the Brick library in Haskell to build the game on a TUI. 

Goals/Stages of project development:

- Code Architecture: Structure of the project, divided into different modules of UI, game states definition, handling events, implementing game algorithm, defining leaderboard.
- Interface module(Board UI design): Develop UI for a fixed board size using brick. 
- Game states: Define the different states for the game (in terms of data types)
- Handling events: Understand how click/keyboard events can be read from the board and mapped to game states.
- Implementing game algorithm: how to randomize bomb placement on the board, implement the minesweeper algorithm (defining perimeter of safe squares)
- Defining leaderboard: saving game states on disk, previous games played by a user, and loading it up and showing on UI everytime the game is run.
- 
- (To do depending on scope and time)
    - Adding more levels to the game (increasing board size, increasing number of bombs in the game) 
    - Saving game state: Intro screen showing play new game or load previous saved game state.
    - Multiplayer minesweeper: 2 players playing minesweeper concurrently/alternatively on the same board maintaining the scores of both users together.
