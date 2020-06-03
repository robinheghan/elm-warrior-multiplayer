module Main exposing (main)

import Player
import Warrior
import Warrior.Map as Map exposing (Map)


main : Program () Warrior.Model Warrior.Msg
main =
    Warrior.multiplayerProgram
        { maps = [ deathmatch ]
        , players =
            [ ( "One", Player.takeTurn )
            , ( "Two", Player.takeTurn )
            , ( "Three", Player.takeTurn )
            , ( "Four", Player.takeTurn )
            , ( "Five", Player.takeTurn )
            , ( "Six", Player.takeTurn )
            , ( "Seven", Player.takeTurn )
            , ( "Eight", Player.takeTurn )
            ]
        , msPerTurn = 1000
        , winCondition = \players _ -> List.length players == 1
        }


deathmatch : Map
deathmatch =
    Map.init { rows = 12, columns = 12 }
        |> Map.withSpawnPoint { x = 2, y = 1 }
        |> Map.withSpawnPoint { x = 2, y = 4 }
        |> Map.withSpawnPoint { x = 2, y = 7 }
        |> Map.withSpawnPoint { x = 2, y = 10 }
        |> Map.withSpawnPoint { x = 9, y = 1 }
        |> Map.withSpawnPoint { x = 9, y = 4 }
        |> Map.withSpawnPoint { x = 9, y = 7 }
        |> Map.withSpawnPoint { x = 9, y = 10 }
