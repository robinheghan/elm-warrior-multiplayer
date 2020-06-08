module Player.Eight exposing (takeTurn)

import Warrior.Direction as Direction
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Action(..), Player)


takeTurn : Player -> Map -> Action
takeTurn player map =
    let
        playerPosition =
            player |> Player.position
        canMove =
            Map.look Direction.Right playerPosition map 
                |> List.head 
                |> Maybe.map ((==) Map.Empty)
                |> Maybe.withDefault False

        canMoveRight =
            Map.look Direction.Right playerPosition map 
                |> List.head 
                |> Maybe.map ((==) Map.Empty)
                |> Maybe.withDefault False

        canMoveLeft =
            Map.look Direction.Left playerPosition map 
                |> List.head 
                |> Maybe.map ((==) Map.Empty)
                |> Maybe.withDefault False

        canMoveUp =
            Map.look Direction.Up playerPosition map 
                |> List.head 
                |> Maybe.map ((==) Map.Empty)
                |> Maybe.withDefault False
        canMoveDown =
            Map.look Direction.Down playerPosition map 
                |> List.head 
                |> Maybe.map ((==) Map.Empty)
                |> Maybe.withDefault False

        canAttackRight =
            Map.look Direction.Right playerPosition map
                |> List.head
                |> Maybe.map ((==) Map.Player)
                |> Maybe.withDefault False

        prevMove =
                Player.previousActions player 
                |> List.head 
                |> Maybe.map Tuple.second 
                |> Maybe.withDefault Player.Wait
        
    in
    if (player |> Player.health) < 3 && canAttackRight then
        Player.Move Direction.Left
    else if (player |> Player.health) < 10 && canMove then
        Player.Heal
    else if canAttackRight then
        Player.Attack Direction.Right
    else if canMoveRight then     
        if prevMove == Player.Move Direction.Right then
            Player.Move Direction.Right
        else if canMoveDown then
            Player.Move Direction.Down
        else if canMoveLeft then
            Player.Move Direction.Left
        else
            Player.Move Direction.Up
    else if canMoveUp then
        if prevMove == Player.Move Direction.Up then
            Player.Move Direction.Up
        else if canMoveLeft then
            Player.Move Direction.Left
        else if canMoveDown then
            Player.Move Direction.Down
        else
            Player.Move Direction.Right
    else if canMoveLeft then
        if prevMove == Player.Move Direction.Left then
            Player.Move Direction.Left
        else if canMoveUp then 
            Player.Move Direction.Up
        else if canMoveRight then
            Player.Move Direction.Right
        else
            Player.Move Direction.Down
    else if canMoveDown then
        if prevMove == Player.Move Direction.Down then
            Player.Move Direction.Down
        else if canMoveRight then
            Player.Move Direction.Right
        else if canMoveLeft then
            Player.Move Direction.Up
        else
            Player.Move Direction.Left
    else
        Player.Wait
