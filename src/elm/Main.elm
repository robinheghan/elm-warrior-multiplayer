module Main exposing (main)

import Player.Eight
import Player.Five
import Player.Four
import Player.One
import Player.Seven
import Player.Six
import Player.Three
import Player.Two
import Warrior.Item as Item
import Warrior.Map.Builder as Map exposing (Template)
import Warrior.Map.Progression as Progression
import Warrior.Program as Warrior


main : Program () Warrior.Model Warrior.Msg
main =
    Warrior.program
        { maps = [ deathmatch ]
        , players =
            [ ( "Robin", Player.One.takeTurn )
            , ( "Tarjei", Player.Two.takeTurn )
            , ( "Bjørn-Ivar", Player.Three.takeTurn )
            , ( "Knut Erik", Player.Four.takeTurn )
            , ( "Mathilde", Player.Five.takeTurn )
            , ( "Aksel", Player.Six.takeTurn )
            , ( "Øyvind", Player.Seven.takeTurn )
            , ( "Rahil", Player.Eight.takeTurn )
            ]
        , msPerTurn = 500
        , progressionFunction = Progression.lastWarriorStanding
        }


deathmatch : Template
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
