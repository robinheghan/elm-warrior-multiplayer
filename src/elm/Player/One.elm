module Player.One exposing (takeTurn)

import Warrior exposing (Action(..), Warrior)
import Warrior.Direction as Direction
import Warrior.History as History exposing (History)
import Warrior.Map as Map exposing (Map)
import Warrior.Map.Tile as Tile


takeTurn : Warrior -> Map -> History -> Action
takeTurn warrior map history =
    let
        canMoveTo direction =
            Map.look direction warrior map
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.map Tile.canMoveOnto
                |> Maybe.withDefault False

        canAttack direction =
            Map.look direction warrior map
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.map Tile.isWarrior
                |> Maybe.withDefault False

        lastAction =
            History.previousActions warrior history
                |> List.head
                |> Maybe.withDefault Wait

        wouldUndoLastMove dir =
            case ( dir, lastAction ) of
                ( Direction.Left, Move Direction.Right ) ->
                    True

                ( Direction.Right, Move Direction.Left ) ->
                    True

                ( Direction.Down, Move Direction.Up ) ->
                    True

                ( Direction.Up, Move Direction.Down ) ->
                    True

                _ ->
                    False

        preferredAction dir =
            if canAttack dir then
                Attack dir

            else
                Move dir
    in
    Direction.all
        |> List.filter (\dir -> canMoveTo dir || canAttack dir)
        |> List.filter (not << wouldUndoLastMove)
        |> List.head
        |> Maybe.map preferredAction
        |> Maybe.withDefault Wait
