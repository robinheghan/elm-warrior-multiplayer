module Player.Two exposing (takeTurn)

import Player.One
import Warrior exposing (Action(..), Warrior)
import Warrior.History as History exposing (History)
import Warrior.Map as Map exposing (Map)


takeTurn : Warrior -> Map -> History -> Action
takeTurn =
    Player.One.takeTurn
