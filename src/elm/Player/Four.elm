module Player.Four exposing (takeTurn)

import Warrior.Direction as Direction
import Warrior.Map as Map exposing (Map, Tile(..))
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

        canAttackInDirection direction =
            Map.look direction currentPosition map
                |> List.head
                |> Maybe.map ((==) Map.Player)
                |> Maybe.withDefault False
                |> (\c -> if c then Just direction else Nothing)
 
        canAttack =
            Direction.all
                |> List.map canAttackInDirection
                |> List.filterMap identity
                |> List.head

        canPickupItem =
            case Map.lookDown player map of 
                    Item item ->
                        True
                    _ ->
                        False

        canHeal = 
            (Player.maxHealth player) > (Player.health player) && (Player.healingPotential player) <= ((Player.maxHealth player) - (Player.health player))

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
            if canPickupItem then
                Pickup        
            
            else
                case canAttack of
                    Just direction ->
                        Attack direction
                    _ ->
                        if canHeal then
                            Heal
                        else
                            Move dir
    in
    [Direction.Up, Direction.Down, Direction.Left, Direction.Right]
        |> List.filter canMoveTo
        |> List.filter (not << wouldUndoLastMove)
        |> List.head
        |> Maybe.map preferredAction
        |> Maybe.withDefault Wait
