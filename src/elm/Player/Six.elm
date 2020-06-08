module Player.Six exposing (takeTurn)

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction(..))
import Warrior.Item exposing (Item(..))
import Warrior.Map as Map exposing (Map, Tile(..))
import Warrior.Player as Player exposing (Action(..), Player)


takeTurn : Player -> Map -> Player.Action
takeTurn player map =
    let
        direction =
            nextDirection player map

        _ =
            Debug.log "player health" (Player.health player)
    in
    if isFacingVillain player map direction then
        if shouldRetreatToHeal player then
            player
                |> Player.previousActions
                |> List.map Tuple.second
                |> directionOfLastPosition
                |> Move

        else if doesNotHaveSword player && onSwordTile player map then
            Pickup

        else
            Attack direction

    else if Player.health player < Player.maxHealth player then
        Heal

    else if onItemTile player map then
        Pickup

    else
        case isInRetreat player of
            Just { directionOfVillain } ->
                Move directionOfVillain

            Nothing ->
                Move direction


onSwordTile : Player -> Map -> Bool
onSwordTile player map =
    case Map.lookDown player map of
        Item Sword ->
            True

        _ ->
            False


doesNotHaveSword : Player -> Bool
doesNotHaveSword player =
    player
        |> Player.inventory
        |> List.member Sword
        |> not


shouldRetreatToHeal : Player -> Bool
shouldRetreatToHeal player =
    if Player.health player == Player.maxHealth player then
        False

    else
        case Player.previousActions player of
            ( lastPlayerState, _ ) :: _ ->
                let
                    enemyAttackPower =
                        --Player.health lastPlayerState - Player.health player
                        3
                in
                Player.health player - enemyAttackPower < 1

            _ ->
                False


isFacingVillain : Player -> Map -> Direction -> Bool
isFacingVillain player map direction =
    Map.look direction (Player.position player) map
        |> List.head
        |> Maybe.map isAnotherPlayer
        |> Maybe.withDefault False


isInRetreat : Player -> Maybe FightInfo
isInRetreat player =
    player
        |> Player.previousActions
        |> includesRetreatSequence


includesRetreatSequence : List ( Player, Action ) -> Maybe FightInfo
includesRetreatSequence previousActions =
    --- Hvis man har attacket, og så gått tilbake til et square man allerede har besøkt er man i retreat
    case previousActions of
        ( lastPlayer, Move lastDirection ) :: ( _, Attack _ ) :: rest ->
            if includesPosition rest (nextTo lastDirection (Player.position lastPlayer)) then
                Just { directionOfVillain = oppositeDirection lastDirection }

            else
                Nothing

        ( _, Heal ) :: rest ->
            includesRetreatSequence rest

        _ ->
            Nothing


includesPosition : List ( Player, Action ) -> Coordinate -> Bool
includesPosition previousActions coordinate =
    previousActions
        |> List.map (Tuple.first >> Player.position)
        |> List.member coordinate


onItemTile : Player -> Map -> Bool
onItemTile player map =
    case Map.lookDown player map of
        Item _ ->
            True

        _ ->
            False


nextMoveIsOppositeDirection : Direction -> List Action -> Bool
nextMoveIsOppositeDirection lastDirection actions =
    case actions of
        (Move direction) :: _ ->
            directionsAreOpposite lastDirection direction

        _ :: rest ->
            nextMoveIsOppositeDirection lastDirection rest

        [] ->
            False


directionOfLastPosition : List Action -> Direction
directionOfLastPosition previousActions =
    case previousActions of
        (Move direction) :: _ ->
            oppositeDirection direction

        [] ->
            Right

        _ :: rest ->
            directionOfLastPosition rest


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Left ->
            Right

        Right ->
            Left

        Up ->
            Down

        Down ->
            Up


directionsAreOpposite : Direction -> Direction -> Bool
directionsAreOpposite direction1 direction2 =
    case ( direction1, direction2 ) of
        ( Left, Right ) ->
            True

        ( Right, Left ) ->
            True

        ( Up, Down ) ->
            True

        ( Down, Up ) ->
            True

        _ ->
            False


type alias FightInfo =
    { directionOfVillain : Direction
    }


isAnotherPlayer : Tile -> Bool
isAnotherPlayer tile =
    case tile of
        Player ->
            True

        _ ->
            False


nextDirection : Player -> Map -> Direction
nextDirection player map =
    case directionOfGoal player map of
        Just direction ->
            direction

        Nothing ->
            moveToNewSquare player map


directionOfGoal : Player -> Map -> Maybe Direction
directionOfGoal player map =
    if isDirectionOfGoal player map Right then
        Just Right

    else if isDirectionOfGoal player map Down then
        Just Down

    else if isDirectionOfGoal player map Left then
        Just Left

    else if isDirectionOfGoal player map Up then
        Just Up

    else
        Nothing


isDirectionOfGoal : Player -> Map -> Direction -> Bool
isDirectionOfGoal player map direction =
    List.any isGoal (Map.look direction (Player.position player) map)


isGoal : Tile -> Bool
isGoal tile =
    case tile of
        Exit ->
            True

        _ ->
            False


moveToNewSquare : Player -> Map -> Direction
moveToNewSquare player map =
    let
        _ =
            2
    in
    if List.length (Player.previousActions player) == 0 then
        Up

    else if hasOnlyMovedUp player then
        Down

    else
        player
            |> previousDirection
            |> Maybe.withDefault Right
            |> allDirectionsClockwiseFromStartingDirection
            |> List.filter (unvisited player)
            |> List.filter (canMoveInDirection player map)
            |> List.head
            |> Maybe.withDefault (moveAimlessly player map)


hasOnlyMovedUp : Player -> Bool
hasOnlyMovedUp player =
    player
        |> Player.previousActions
        |> List.map Tuple.second
        |> onlyIncludesUpwardMoves


onlyIncludesUpwardMoves : List Action -> Bool
onlyIncludesUpwardMoves actions =
    case actions of
        (Move Up) :: rest ->
            onlyIncludesUpwardMoves rest

        (Move _) :: rest ->
            False

        _ :: rest ->
            onlyIncludesUpwardMoves rest

        [] ->
            True


canMoveInDirection : Player -> Map -> Direction -> Bool
canMoveInDirection player map direction =
    Map.look direction (Player.position player) map
        |> List.head
        |> Maybe.map emptyOrEnemy
        |> Maybe.withDefault False


emptyOrEnemy : Tile -> Bool
emptyOrEnemy tile =
    case tile of
        Wall ->
            False

        Empty ->
            True

        SpawnPoint ->
            True

        Player ->
            True

        Item _ ->
            True

        Exit ->
            True


unvisited : Player -> Direction -> Bool
unvisited player direction =
    player
        |> Player.previousActions
        |> List.map (Tuple.first >> Player.position)
        |> List.member (nextTo direction (Player.position player))
        |> not


previousDirection : Player -> Maybe Direction
previousDirection player =
    case (Player.previousActions >> List.head) player of
        Just ( _, Move direction ) ->
            Just direction

        _ ->
            Nothing


moveAimlessly : Player -> Map -> Direction
moveAimlessly player map =
    case (Player.previousActions >> List.head) player of
        Just ( _, Move direction ) ->
            tryAllDirectionsClockWise direction player map

        _ ->
            tryAllDirectionsClockWise Right player map


tryAllDirectionsClockWise : Direction -> Player -> Map -> Direction
tryAllDirectionsClockWise firstDirection player map =
    if canMoveInDirection player map firstDirection then
        firstDirection

    else if canMoveInDirection player map (nextDirectionClockwise firstDirection) then
        nextDirectionClockwise firstDirection

    else if canMoveInDirection player map (nextDirectionClockwise (nextDirectionClockwise firstDirection)) then
        nextDirectionClockwise (nextDirectionClockwise firstDirection)

    else
        nextDirectionClockwise (nextDirectionClockwise (nextDirectionClockwise firstDirection))


allDirectionsClockwiseFromStartingDirection : Direction -> List Direction
allDirectionsClockwiseFromStartingDirection direction =
    [ direction
    , nextDirectionClockwise direction
    , direction |> nextDirectionClockwise |> nextDirectionClockwise
    , direction |> nextDirectionClockwise |> nextDirectionClockwise |> nextDirectionClockwise
    ]


nextDirectionClockwise : Direction -> Direction
nextDirectionClockwise direction =
    case direction of
        Right ->
            Down

        Down ->
            Left

        Left ->
            Up

        Up ->
            Right


nextTo : Direction -> Coordinate -> Coordinate
nextTo direction { x, y } =
    case direction of
        Left ->
            { x = x - 1
            , y = y
            }

        Right ->
            { x = x + 1
            , y = y
            }

        Up ->
            { x = x
            , y = y - 1
            }

        Down ->
            { x = x
            , y = y + 1
            }
