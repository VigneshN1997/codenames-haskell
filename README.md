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

Updates:

- Developed the code architecture and the UI.
![Spy View](resources/images/spyview.png?raw=true "Spy View")
