module Main exposing (main)

import Player
import Warrior
import Warrior.Item as Item
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
    Map.init { rows = 12, columns = 13 }
        |> Map.withWalledArea
            { x = 6, y = 3 }
            { x = 6, y = 8 }
        |> Map.withSpawnPoint { x = 2, y = 1 }
        |> Map.withSpawnPoint { x = 2, y = 4 }
        |> Map.withSpawnPoint { x = 2, y = 7 }
        |> Map.withSpawnPoint { x = 2, y = 10 }
        |> Map.withSpawnPoint { x = 10, y = 1 }
        |> Map.withSpawnPoint { x = 10, y = 4 }
        |> Map.withSpawnPoint { x = 10, y = 7 }
        |> Map.withSpawnPoint { x = 10, y = 10 }
        |> Map.withItem { x = 2, y = 0 } Item.Potion
        |> Map.withItem { x = 2, y = 3 } Item.Potion
        |> Map.withItem { x = 2, y = 6 } Item.Potion
        |> Map.withItem { x = 2, y = 9 } Item.Potion
        |> Map.withItem { x = 10, y = 0 } Item.Potion
        |> Map.withItem { x = 10, y = 3 } Item.Potion
        |> Map.withItem { x = 10, y = 6 } Item.Potion
        |> Map.withItem { x = 10, y = 9 } Item.Potion
        |> Map.withItem { x = 2, y = 2 } Item.Sword
        |> Map.withItem { x = 2, y = 5 } Item.Sword
        |> Map.withItem { x = 2, y = 8 } Item.Sword
        |> Map.withItem { x = 2, y = 11 } Item.Sword
        |> Map.withItem { x = 10, y = 2 } Item.Sword
        |> Map.withItem { x = 10, y = 5 } Item.Sword
        |> Map.withItem { x = 10, y = 8 } Item.Sword
        |> Map.withItem { x = 10, y = 11 } Item.Sword
