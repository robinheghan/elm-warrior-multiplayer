module Player.Seven exposing (takeTurn)

import Dict
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.Map exposing (Map)
import Warrior.Player as Player exposing (Player)


nextCoordinate : Coordinate -> Direction -> Coordinate
nextCoordinate { x, y } dir =
    case dir of
        Direction.Left ->
            { x = x - 1, y = y }

        Direction.Right ->
            { x = x + 1, y = y }

        Direction.Down ->
            { x = x, y = y + 1 }

        Direction.Up ->
            { x = x, y = y - 1 }


oppositeDirection : Direction -> Direction
oppositeDirection dir =
    case dir of
        Direction.Left ->
            Direction.Right

        Direction.Right ->
            Direction.Left

        Direction.Up ->
            Direction.Down

        Direction.Down ->
            Direction.Up


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if predicate x then
                x :: takeWhile predicate xs

            else
                []


coordinateToTuple : Coordinate -> ( Int, Int )
coordinateToTuple { x, y } =
    ( x, y )


tupleToCoordinate : ( Int, Int ) -> Coordinate
tupleToCoordinate ( x, y ) =
    { x = x, y = y }


type alias Coord =
    ( Int, Int )


type alias CandidateAction =
    Maybe Player.Action


bfsForTargetTile : Warrior.Map.Tile -> Player -> Map -> CandidateAction
bfsForTargetTile target player map =
    let
        bfs : Dict.Dict Coord Player.Action -> Dict.Dict Coord Warrior.Map.Tile -> List ( Coord, Player.Action ) -> ( Dict.Dict Coord Warrior.Map.Tile, Dict.Dict Coord Player.Action )
        bfs visited currentMap toExplore =
            let
                newCoordinates pos =
                    Direction.all
                        |> List.map (\dir -> Maybe.map (\( coordinate, tile ) -> ( ( coordinateToTuple coordinate, tile ), Player.Move dir )) (List.head <| lookWithCoordinatesAndTiles map pos dir))
                        |> List.filterMap identity
                        |> List.filter (\( ( coord, _ ), _ ) -> not <| Dict.member coord visited)

                newCoordinatesToExplore pos =
                    newCoordinates pos
                        |> List.map (\( ( coord, _ ), move ) -> ( coord, move ))

                newMapExplored pos =
                    newCoordinates pos
                        |> List.map Tuple.first

                explore : Coord -> ( List ( Coord, Player.Action ), Dict.Dict Coord Warrior.Map.Tile )
                explore pos =
                    if Dict.member pos visited || Dict.get pos currentMap == Just target then
                        ( [], Dict.empty )

                    else
                        ( newCoordinatesToExplore (tupleToCoordinate pos), Dict.fromList (newMapExplored (tupleToCoordinate pos)) )

                newMap pos =
                    Dict.union currentMap <| Tuple.second (explore pos)

                newExploreLocations pos =
                    List.append (Maybe.withDefault [] (List.tail toExplore)) <| Tuple.first (explore pos)
            in
            case toExplore of
                [] ->
                    ( currentMap, visited )

                ( pos, move ) :: _ ->
                    bfs (Dict.insert pos move visited) (newMap pos) (newExploreLocations pos)

        playerPosition =
            Player.position player

        completedMapAndSeen =
            bfs Dict.empty Dict.empty [ ( coordinateToTuple playerPosition, Player.Wait ) ]

        completedMap : Dict.Dict Coord Warrior.Map.Tile
        completedMap =
            Tuple.first completedMapAndSeen

        completedSeen =
            Tuple.second completedMapAndSeen

        targetTileCoordinate =
            Dict.keys completedMap
                |> List.map (\coord -> ( coord, Dict.get coord completedMap ))
                |> List.filter (\( _, tile ) -> tile == Just target)
                |> List.map (\( coord, _ ) -> coord)
                |> List.filter ((/=) (coordinateToTuple playerPosition))
                |> List.head

        reversePath cumulative coordinate =
            case Dict.get coordinate completedSeen of
                Just Player.Wait ->
                    cumulative

                Just (Player.Move dir) ->
                    reversePath (Player.Move dir :: cumulative) (coordinateToTuple <| nextCoordinate (tupleToCoordinate coordinate) (oppositeDirection dir))

                _ ->
                    cumulative
    in
    Maybe.map (reversePath []) targetTileCoordinate
        |> Maybe.andThen List.head


lookWithCoordinatesAndTiles : Map -> Coordinate -> Direction -> List ( Coordinate, Warrior.Map.Tile )
lookWithCoordinatesAndTiles map pos dir =
    let
        tiles =
            Warrior.Map.look dir pos map

        count =
            List.length tiles

        combined =
            List.map2 Tuple.pair (lookWithCoordinatesInternal pos dir count) tiles

        lookWithCoordinatesInternal posi dire cnt =
            let
                coordinate =
                    nextCoordinate posi dire
            in
            if cnt <= 0 then
                []

            else
                coordinate :: lookWithCoordinatesInternal coordinate dire (cnt - 1)
    in
    takeWhile (\( _, tile ) -> tile /= Warrior.Map.Wall) combined


lookWithCoordinates : Map -> Coordinate -> Direction -> List Coordinate
lookWithCoordinates map pos dir =
    lookWithCoordinatesAndTiles map pos dir |> List.map Tuple.first


takeTurn : Player -> Map -> Player.Action
takeTurn player map =
    let
        playerPosition =
            Player.position player

        canMove direction =
            Warrior.Map.look direction playerPosition map
                |> List.head
                |> Maybe.map Warrior.Map.canMoveOntoTile
                |> Maybe.withDefault False

        bfsToExit =
            bfsForTargetTile Warrior.Map.Exit player map

        huntOtherPlayers =
            bfsForTargetTile Warrior.Map.Player player map

        -- preprogrammed moves :o
        preprogrammedMoves =
            case List.length (Player.previousActions player) of
                0 ->
                    Just (Player.Move Direction.Down)

                1 ->
                    Just Player.Pickup

                _ ->
                    Nothing

        visitedCoordinates =
            Player.previousActions player
                |> List.map (\( p, _ ) -> Player.position p)

        hasNotMovedBefore dir =
            lookWithCoordinates map playerPosition dir
                |> List.all (\coord -> List.member coord visitedCoordinates)
                |> not

        validMoves =
            List.filter canMove Direction.all

        newDirectionMove =
            validMoves
                |> List.filter hasNotMovedBefore
                |> List.map Player.Move
                |> List.head

        -- backtrackMove = Just <| Player.Move Direction.Left
        enemyNextToUs dir =
            Warrior.Map.look dir playerPosition map
                |> List.head
                |> Maybe.map ((==) Warrior.Map.Player)
                |> Maybe.withDefault False

        pickupItem =
            case Warrior.Map.lookDown player map of
                Warrior.Map.Item _ ->
                    Just Player.Pickup
                    
                _ ->
                    Nothing

        anyEnemyNextToUs =
            Direction.all
                |> List.map enemyNextToUs
                |> List.any identity

        lowHealth =
            Player.health player <= Player.maxHealth player // 2

        heal =
            if Player.health player < Player.maxHealth player && not anyEnemyNextToUs then
                Just Player.Heal

            else
                Nothing

        enemyCloseToUs dir =
            Warrior.Map.look dir playerPosition map
                |> List.take 1
                |> List.any ((==) Warrior.Map.Player)

        runAwayOpposite =
            if lowHealth then
                Direction.all
                    |> List.filter enemyCloseToUs
                    |> List.map oppositeDirection
                    |> List.filter (\m -> List.member m validMoves)
                    |> List.map Player.Move
                    |> List.head

            else
                Nothing

        noEnemiesClose position dirsToCheck =
            dirsToCheck
                |> List.map (\dir -> Warrior.Map.look dir position map)
                |> List.map List.head
                |> List.filterMap identity
                |> List.any ((==) Warrior.Map.Player)
                |> not

        runAwayAny =
            if lowHealth && anyEnemyNextToUs then
                Player.previousActions player
                    |> List.filterMap
                        (\( _, action ) ->
                            case action of
                                Player.Move dir ->
                                    if noEnemiesClose (nextCoordinate playerPosition (oppositeDirection dir)) (Direction.all |> List.filter ((/=) dir)) then
                                        Just (Player.Move (oppositeDirection dir))

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                    |> List.head

            else
                Nothing

        fight =
            Direction.all
                |> List.filter enemyNextToUs
                |> List.map Player.Attack
                |> List.head

        candidateActions =
            [ preprogrammedMoves, runAwayOpposite, runAwayAny, heal, pickupItem, fight, huntOtherPlayers, bfsToExit, newDirectionMove ]
    in
    candidateActions
        |> List.filterMap identity
        |> List.head
        |> Maybe.withDefault Player.Wait
