open! Core

let command =
  Command.group
    ~summary:"A tool for playing the wikipedia game, and other utilities"
    [ "wiki-game", Wiki_game.command
    ; "interstate", Interstate.command
    ; "social-network", Social_network.command
    ; "lambda-soup-utilities", Lambda_soup_utilities.command
    ; "imdb", Imdb.command
    ; "maze", Maze.command
    ; "file-fetcher-demo", File_fetcher_demo.command
    ]
;;
