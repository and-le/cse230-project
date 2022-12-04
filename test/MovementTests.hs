module MovementTests
  ( movementTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Sokoban

movementTests =
  testGroup "Movement tests" [basicMovement, pushMovement, recursiveMovement]

basicMovement =
  testGroup
    "Basic movement"
    [ test1
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    , test11
    , test12
    ]

pushMovement =
  testGroup "Push movement" [test13, test14, test15, test16, test17, test18]

recursiveMovement = testGroup "Recursive movement" [test19, test20]

-- Basic movement
test1 =
  testCase "Move UP to an EMPTY cell" $
  (move UpMv (fromCells [[emptyCell, emptyCell], [emptyCell, playerCell]])) @?=
  fromCells [[emptyCell, playerCell], [emptyCell, emptyCell]]

test2 =
  testCase "Move RIGHT to an EMPTY cell" $
  (move RightMv (fromCells [[playerCell, emptyCell], [emptyCell, emptyCell]])) @?=
  fromCells [[emptyCell, playerCell], [emptyCell, emptyCell]]

test3 =
  testCase "Move DOWN to an EMPTY cell" $
  (move DownMv (fromCells [[emptyCell, playerCell], [emptyCell, emptyCell]])) @?=
  fromCells [[emptyCell, emptyCell], [emptyCell, playerCell]]

test4 =
  testCase "Move LEFT to an EMPTY cell" $
  (move LeftMv (fromCells [[emptyCell, playerCell], [emptyCell, emptyCell]])) @?=
  fromCells [[playerCell, emptyCell], [emptyCell, emptyCell]]

test5 =
  testCase "Move UP to a WALL cell" $
  (move UpMv (fromCells [[emptyCell, wallCell], [emptyCell, playerCell]])) @?=
  fromCells [[emptyCell, wallCell], [emptyCell, playerCell]]

test6 =
  testCase "Move RIGHT to a WALL cell" $
  (move RightMv (fromCells [[playerCell, wallCell], [emptyCell, emptyCell]])) @?=
  fromCells [[playerCell, wallCell], [emptyCell, emptyCell]]

test7 =
  testCase "Move DOWN to a WALL cell" $
  (move DownMv (fromCells [[emptyCell, playerCell], [emptyCell, wallCell]])) @?=
  fromCells [[emptyCell, playerCell], [emptyCell, wallCell]]

test8 =
  testCase "Move LEFT to a WALL cell" $
  (move LeftMv (fromCells [[wallCell, playerCell], [emptyCell, emptyCell]])) @?=
  fromCells [[wallCell, playerCell], [emptyCell, emptyCell]]

test9 = testCase "Move UP out-of-bounds" $ (move UpMv env) @?= env
  where
    env = fromCells [[emptyCell, playerCell], [emptyCell, emptyCell]]

test10 = testCase "Move RIGHT out-of-bounds" $ (move RightMv env) @?= env
  where
    env = fromCells [[emptyCell, playerCell], [emptyCell, emptyCell]]

test11 = testCase "Move DOWN out-of-bounds" $ (move DownMv env) @?= env
  where
    env = fromCells [[emptyCell, emptyCell], [playerCell, emptyCell]]

test12 = testCase "Move LEFT out-of-bounds" $ (move LeftMv env) @?= env
  where
    env = fromCells [[emptyCell, emptyCell], [playerCell, emptyCell]]

-- Pushing movement
test13 =
  testCase "Move UP and push TRASH" $
  (move
     UpMv
     (fromCells
        [ [emptyCell, emptyCell, emptyCell]
        , [emptyCell, trashCell, emptyCell]
        , [emptyCell, playerCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, trashCell, emptyCell]
    , [emptyCell, playerCell, emptyCell]
    , [emptyCell, emptyCell, emptyCell]
    ]

test14 =
  testCase "Move DOWN and push TRASH" $
  (move
     DownMv
     (fromCells
        [ [emptyCell, playerCell, emptyCell]
        , [emptyCell, trashCell, emptyCell]
        , [emptyCell, emptyCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, emptyCell]
    , [emptyCell, playerCell, emptyCell]
    , [emptyCell, trashCell, emptyCell]
    ]

test15 =
  testCase "Move RIGHT and push TRASH" $
  (move
     RightMv
     (fromCells
        [ [emptyCell, emptyCell, emptyCell]
        , [playerCell, trashCell, emptyCell]
        , [emptyCell, emptyCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, emptyCell]
    , [emptyCell, playerCell, trashCell]
    , [emptyCell, emptyCell, emptyCell]
    ]

test16 =
  testCase "Move LEFT and push TRASH" $
  (move
     LeftMv
     (fromCells
        [ [emptyCell, emptyCell, emptyCell]
        , [emptyCell, trashCell, playerCell]
        , [emptyCell, emptyCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, emptyCell]
    , [trashCell, playerCell, emptyCell]
    , [emptyCell, emptyCell, emptyCell]
    ]

test17 =
  testCase "Player moves into stash and player does not disappear" $
  (move
     DownMv
     (fromCells
        [ [emptyCell, emptyCell, wallCell]
        , [playerCell, emptyCell, emptyCell]
        , [stashCell, trashCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, wallCell]
    , [playerCell, emptyCell, emptyCell]
    , [stashCell, trashCell, emptyCell]
    ]

test18 =
  testCase "Player pushes trash into stash and trash disappears" $
  (move
     LeftMv
     (fromCells
        [ [emptyCell, emptyCell, wallCell]
        , [emptyCell, emptyCell, emptyCell]
        , [stashCell, trashCell, playerCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [stashCell, playerCell, emptyCell]
    ]

-- Recursive pushing
test19 =
  testCase "Recursive push" $
  (move
     UpMv
     (fromCells
        [ [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
        , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
        , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
        , [wallCell, wallCell, trashCell, emptyCell, emptyCell]
        , [wallCell, wallCell, playerCell, emptyCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, trashCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [wallCell, wallCell, playerCell, emptyCell, emptyCell]
    , [wallCell, wallCell, emptyCell, emptyCell, emptyCell]
    ]

test20 =
  testCase "Recursive push into stash" $
  (move
     DownMv
     (fromCells
        [ [emptyCell, emptyCell, playerCell, emptyCell, emptyCell]
        , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
        , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
        , [wallCell, wallCell, trashCell, emptyCell, emptyCell]
        , [wallCell, wallCell, stashCell, emptyCell, emptyCell]
        ])) @?=
  fromCells
    [ [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, playerCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [wallCell, wallCell, trashCell, emptyCell, emptyCell]
    , [wallCell, wallCell, stashCell, emptyCell, emptyCell]
    ]
