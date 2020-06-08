module Player.Two exposing (takeTurn)

import Warrior.Direction as Direction
import Warrior.Item as Item exposing (Item(..))
import Warrior.Map as Map exposing (Map, Tile(..), look, lookDown)
import Warrior.Player as Player exposing (Action(..), Player, healingPotential, health, inventory, maxHealth, position)


takeTurn : Player -> Map -> Action
takeTurn player map =
    let
        currentPosition =
            player |> position

        canMoveTo direction =
            Map.look direction currentPosition map
                |> List.head
                |> Maybe.map Map.canMoveOntoTile
                |> Maybe.withDefault False

        findFirstDirectionThat pred dirs =
            dirs
                |> List.map (\dir -> look dir (player |> position) map)
                |> List.map pred
                |> List.map2 Tuple.pair dirs
                |> List.filterMap thatDirectionIsGood
                |> List.head

        canSee tile tiles =
            let
                walkableTiles =
                    [ Empty, SpawnPoint, Exit ]
            in
            tiles
                |> List.filter (not << flip List.member walkableTiles)
                |> List.head
                |> Maybe.map ((==) tile)
                |> Maybe.withDefault False

        directionOfPlayer =
            Direction.all
                |> findFirstDirectionThat (canSee Player)

        directionOfPotion =
            if inventory player |> List.member Potion then
                Nothing

            else
                Direction.all
                    |> findFirstDirectionThat (canSee (Potion |> Item))

        directionOfSword =
            if inventory player |> List.member Sword then
                Nothing

            else
                Direction.all
                    |> findFirstDirectionThat (canSee (Sword |> Item))

        directionWhichCanAttack =
            Direction.all
                |> findFirstDirectionThat (List.head >> Maybe.map ((==) Player) >> Maybe.withDefault False)

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
            if lookDown player map == (Sword |> Item) then
                Pickup

            else if lookDown player map == (Potion |> Item) then
                Pickup

            else if healingPotential player > 0 && health player < maxHealth player then
                Heal

            else if canAttack dir then
                Attack dir

            else
                Move dir
    in
    [ directionOfSword, directionOfPotion, directionWhichCanAttack, directionOfPlayer ]
        |> List.filterMap (\x -> x)
        |> List.head
        |> Maybe.map preferredAction
        |> Maybe.withDefault Wait


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x



-- let
--     findFirstDirectionThat pred dirs =
--         let
--             tileNextToPlayer direction =
--                 look direction (player |> position) map |> List.head |> Maybe.withDefault Wall
--         in
--         dirs
--             |> List.map tileNextToPlayer
--             |> List.map pred
--             |> List.map2 Tuple.pair dirs
--             |> List.filterMap thatDirectionIsGood
--             |> List.head
--     previousPos =
--         previousActions player
--             |> List.head
--             |> Maybe.map Tuple.first
--             |> Maybe.map position
--             |> Maybe.withDefault
--                 { x = 0, y = 0 }
--     currentPos =
--         player |> position
--     directionVector =
--         { x = currentPos.x - previousPos.x
--         , y = currentPos.y - previousPos.y
--         }
--     timesToRotate =
--         if directionVector.y < 0 then
--             0
--         else if directionVector.x > 0 then
--             1
--         else if directionVector.y > 0 then
--             2
--         else
--             3
--     directionsRelativeToPlayer =
--         -- Follow the well known maze strategy
--         -- Keep to the left!
--         [ Left, Up, Right, Down ]
--             |> rotateList timesToRotate
--     firstMovableDirectionToTheLeftRelativeToPlayer =
--         findFirstDirectionThat canMoveOntoTile directionsRelativeToPlayer
--     anyDirection =
--         [ Left, Up, Down, Right ]
--     directionOfExitNextToPlayer =
--         findFirstDirectionThat isExit anyDirection
-- in
-- [ directionOfExitNextToPlayer
-- , firstMovableDirectionToTheLeftRelativeToPlayer
-- ]
--     |> List.filterMap (\x -> x)
--     |> List.head
--     |> Maybe.withDefault Right
--     |> Player.Move


isExit : Tile -> Bool
isExit tile =
    case tile of
        Exit ->
            True

        _ ->
            False


thatDirectionIsGood : ( Direction.Direction, Bool ) -> Maybe Direction.Direction
thatDirectionIsGood ( direction, canMoveThere ) =
    if canMoveThere then
        Just direction

    else
        Nothing


rotateList : Int -> List a -> List a
rotateList n list =
    case list of
        [] ->
            []

        x :: xs ->
            case n of
                0 ->
                    list

                _ ->
                    rotateList (n - 1) (xs ++ [ x ])
