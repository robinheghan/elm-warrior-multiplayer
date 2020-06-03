module Player.Five exposing (takeTurn)

import Warrior.Direction as Direction
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Action(..), Player)


takeTurn : Player -> Map -> Action
takeTurn player map =
    let
        currentPosition =
            Player.position player

        canMoveTo direction =
            Map.look direction currentPosition map
                |> List.head
                |> Maybe.map Map.canMoveOntoTile
                |> Maybe.withDefault False

        canAttack direction =
            Map.look direction currentPosition map
                |> List.head
                |> Maybe.map ((==) Map.Player)
                |> Maybe.withDefault False

        lastAction =
            Player.previousActions player
                |> List.head
                |> Maybe.map Tuple.second
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
        |> List.filter canMoveTo
        |> List.filter (not << wouldUndoLastMove)
        |> List.head
        |> Maybe.map preferredAction
        |> Maybe.withDefault Wait
