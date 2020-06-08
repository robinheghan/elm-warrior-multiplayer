module Player.Five exposing (takeTurn)

import Warrior.Map as Map exposing (Map)
import Warrior.Map as Tile exposing (Tile)
import Warrior.Player as Player exposing (Player)
import Warrior.Direction as Direction exposing (Direction)

takeTurn : Player -> Map -> Player.Action
takeTurn player map =
    let position = Player.position player
        previousDirections = Player.previousActions player  
        nextDirection = 
            player 
            |> Player.previousActions
            |> List.head
            |> Maybe.map Tuple.second
        lastMovementDirection = case nextDirection of
            Just action -> action
            Nothing -> Player.Move Direction.Right

        availableDirection = nextAvailableDirection player map lastMovementDirection

    in
    if lastMovementDirection == Player.Move Direction.Left
        then if canMoveToDirection player map Direction.Left
            then Player.Move Direction.Left
        else if availableDirection /= Direction.Right 
            then Player.Move availableDirection
        else Player.Move Direction.Right
    else if lastMovementDirection == Player.Move Direction.Right 
        then if canMoveToDirection player map Direction.Right
            then Player.Move Direction.Right
        else if availableDirection /= Direction.Left
            then Player.Move availableDirection
        else Player.Move Direction.Right
    else if lastMovementDirection == Player.Move Direction.Up
        then if canMoveToDirection player map Direction.Up
            then Player.Move Direction.Up
       else if availableDirection /= Direction.Down 
            then Player.Move availableDirection
        else Player.Move Direction.Right
    else if lastMovementDirection == Player.Move Direction.Down
        then if canMoveToDirection player map Direction.Down
            then Player.Move Direction.Down
        else if availableDirection /= Direction.Up 
            then Player.Move availableDirection
        else Player.Move Direction.Right
    else 
        Player.Move Direction.Right

nextAvailableDirection: Player -> Map -> Player.Action -> Direction 
nextAvailableDirection player map action = 
    let position = Player.position player
        tilesRight = Map.look Direction.Right position map
        tilesDown = Map.look Direction.Down position map
        tilesUp = Map.look Direction.Up position map
        tilesLeft = Map.look Direction.Left position map
        firstTileRight = case List.head tilesRight of
            Just tile -> tile
            Nothing -> Tile.Empty  
        firstTileDown = case List.head tilesDown of
            Just tile -> tile
            Nothing -> Tile.Empty  
        firstTileUp = case List.head tilesUp of
            Just tile -> tile
            Nothing -> Tile.Empty  
        firstTileLeft = case List.head tilesLeft of
            Just tile -> tile
            Nothing -> Tile.Empty  
    in 
    if firstTileRight == Tile.Exit 
        then Direction.Right
    else if firstTileLeft == Tile.Exit 
        then Direction.Left
    else if firstTileUp == Tile.Exit 
        then Direction.Up
    else if firstTileDown == Tile.Exit 
        then Direction.Down
    else if firstTileRight == Tile.Empty && action /= Player.Move Direction.Left 
        then Direction.Right
    else if firstTileLeft == Tile.Empty && action /= Player.Move Direction.Right 
        then Direction.Left
    else if firstTileDown == Tile.Empty && action /= Player.Move Direction.Up
        then Direction.Down
    else if firstTileUp == Tile.Empty && action /= Player.Move Direction.Down 
        then Direction.Up
    else Direction.Right

canMoveToDirection : Player -> Map -> Direction -> Bool
canMoveToDirection player map direction =
    let position = Player.position player
        tilesInDirection = Map.look direction position map 

        firstTileInDirection = case List.head tilesInDirection of
            Just tile -> tile
            Nothing -> Tile.Empty 

    in
    if firstTileInDirection == Tile.Wall
        then False
            
        else 
            True
