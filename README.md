# Codenames-Haskell

We will be developing the popular board game Codenames. The game has two teams competing by each having a "spymaster" give one-word clues that can point to multiple words on the board. The other players on the team attempt to guess their team's words while avoiding the words of the other team.
2 laptops will be connected (one having the spymaster view and one with the player view) So the game states need to be communicated between the machines.

We will be using the Brick library in Haskell to build the game on a TUI. 

Goals/Stages of project development:

- Code Architecture: Structure of the project, divided into different modules of UI, game states definition, handling events, implementing game algorithm, defining team scores and leaderboard.
- Interface module(Board UI design): Develop UI for the spymaster view and the player view. 
- Game states: Define the different states for the game (in terms of data types)
- Networking: There will be two machines connected, one with the player view and one with the spymaster view. The spymaster will give the word hint and the number of words to guess and the player will get this hint and guess words. If the guess is correct, update the card on the player view accordingly and also send the communication to spymaster view for the corresponding player guesses.
- Handling events: Understand how click/keyboard events can be read from the board and mapped to game states.
- Implementing game algorithm: logic of selecting cards : correct guess/incorrect guess, when a game ends, when a team wins?
- Defining team scores: updating team scores on guessing correctly
- Generating word lists from given set of words to be used for a game (File IO to read word lists from persistent storage).
- (To do depending on scope and time)
    - Saving game state: Intro screen showing play new game or load previous saved game state.

# How to run
    
    stack install

Generates executables

- codenames-haskell-exe
- codenames-player-exe

Run both executables on separate terminals.

# Updates:

- Developed the code architecture and the UI.
- Spy View
![Spy View](resources/images/spyview.png?raw=true "Spy View")
- Player View
![Player View](resources/images/playerview.png?raw=true "Player View")

# Files overview

simplified-client.hs 
- connect to server
- receive file number to load
- load the word and colors file
- create the brick UI for the player


simplified-server.hs 
- setup the server
- generate a random file number to play
- send the file number to the client
- load up the brick UI for the spymaster

Game.hs
- contains the spy game state and the player game states
- contains functions to update the game states for both player and spymaster

Common.hs
- commmon functions for sending and receiving messages
- file loading


UI

Styles.hs
- define different styles for different cell types

SpyBoard.hs
- spy board UI
- drawing spy grid, player scores
- drawing the input hint editor
- drawing the current player's turn

PlayerBoard.hs
- Player board UI
- drawing player grid, player scores
- drawing the current player's turn
- showing logs of the cells selected by the players

GameUI.hs
- draw game corresponding on the type of view (player/view)

