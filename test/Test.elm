module Main exposing (..)

import Data.Integer exposing (..)
import ElmTest exposing (..)
import Check exposing (quickCheck, Claim, claim, that, is, for)
import Check.Test
import Check.Producer exposing (int, tuple, rangeInt)
import Maybe exposing (Maybe)


integer : Check.Producer.Producer Integer
integer = Check.Producer.map (\a -> fromInt a) int


addTests : Test
addTests =
    let one = fromInt 1
        two = fromInt 2
        three = fromInt 3
    in
    suite "Add testsuite"
        [ equals (one `add` two) three
        ]

qcAdd : Claim
qcAdd =
  Check.suite "Quickcheck Add"
    [ claim "Conmutative adding"
      `that` (\(a, b) -> a `add` b)
      `is` (\(a, b) -> b `add` a)
      `for` tuple (integer, integer)
    ]


negateTests : Test
negateTests =
    let one = fromInt 1 in
    suite "Negate testsuite"
        [ equals (Data.Integer.negate (fromInt 1)) (fromInt -1)
        , equals (Data.Integer.negate (fromInt -1)) (fromInt 1)
        ]

qcNegate : Claim
qcNegate =
  Check.suite "Quickcheck Negate"
    [ claim "Double negate is noop"
      `that` (\a -> (Data.Integer.negate (Data.Integer.negate a)))
      `is` (\a -> a)
      `for` integer
    ]

subTests : Test
subTests =
    let one = fromInt 1
        two = fromInt 2
        three = fromInt 3
    in
    suite "Sub testsuite"
        [ equals (three `sub` two) one
        ]

qcSub : Claim
qcSub =
  Check.suite "Quickcheck Sub"
    [ claim "Conmutative substract"
      `that` (\(a, b) -> a `sub` b)
      `is` (\(a, b) -> (Data.Integer.negate (b `sub` a)))
      `for` tuple (integer, integer)
    ]

mulTests : Test
mulTests =
    let six = fromInt 6
        two = fromInt 2
        three = fromInt 3
    in
    suite "Mul testsuite"
        [ equals (three `mul` two) six
        , equals (three `mul` (Data.Integer.negate two)) (Data.Integer.negate six)
        ]

qcMul : Claim
qcMul =
  Check.suite "Quickcheck Mul"
    [ claim "Conmutative multiplication"
      `that` (\(a, b) -> a `mul` b)
      `is` (\(a, b) -> b `mul` a)
      `for` tuple (integer, integer)
    ]

divmodTests : Test
divmodTests =
    suite "divmod testsuite"
        [ equals ((fromInt 2000000001) `divmod` (fromInt 2)) (Just (fromInt 1000000000, fromInt 1))
        , equals ((fromInt 2000000002) `divmod` (fromInt 2)) (Just (fromInt 1000000001, fromInt 0))
        , equals ((fromInt 20) `divmod` (fromInt 0)) Nothing
        ]

qcDivMod : Claim
qcDivMod =
  Check.suite "Quickcheck divmod"
    [ claim "divmod definition"
      `that` (\(a, b) ->
            let (c, r) = a `unsafeDivmod` b in
            (c `mul` b) `add` r
        )
      `is` (\(a, b) -> a)
      `for` tuple (integer, Check.Producer.filter (\x -> x `neq` zero) integer)
    ]

qcAbs : Claim
qcAbs =
  Check.suite "Quickcheck abs"
    [ claim "abs is always positive"
      `that` (\a -> (Data.Integer.abs a) `gte` (fromInt 0))
      `is` (\a -> True)
      `for` integer
    , claim "abs definition"
      `that` (\a ->
            if a `gte` (fromInt 0)
            then ((Data.Integer.abs a) `eq` a)
            else ((Data.Integer.abs a) `eq` (Data.Integer.negate a))
        )
      `is` (\a -> True)
      `for` integer
    ]

qcSign : Claim
qcSign =
  Check.suite "Quickcheck sign"
    [ claim "sign definition"
      `that` (\a -> if a `gte` zero then Positive else Negative)
      `is` (\a -> sign a)
      `for` integer
    ]

toStringTests : Test
toStringTests =
    suite "toString testsuite"
        [ equals (Data.Integer.toString (fromInt 3345)) "3345"
        , equals (Data.Integer.toString (fromInt (-3345))) "-3345"
        ]

fromStringTests : Test
fromStringTests =
    suite "fromString testsuite"
        [ equals (Just (fromInt 1)) (fromString "1")
        , equals (Just (fromInt -1)) (fromString "-1")
        , equals (fromString "-a") Nothing
        , equals (fromString "1234567890") (Just (fromInt 1234567890))
        , equals (fromString "+1234567890") (Just (fromInt 1234567890))
        ]

minMaxTests : Test
minMaxTests =
    suite "min-max testsuite"
        [ equals (Data.Integer.max (fromInt 1234567890) (fromInt 3)) (fromInt 1234567890)
        , equals (Data.Integer.max (fromInt 1) (fromInt 3)) (fromInt 3)
        , equals (Data.Integer.min (fromInt 1234567890) (fromInt 3)) (fromInt 3)
        , equals (Data.Integer.min (fromInt 1) (fromInt 3)) (fromInt 1)
        ]

max_digit_valueTests : Test
max_digit_valueTests =
    suite "max_digit_value testsuite"
        [ defaultTest ((max_digit_value * max_digit_value)
                        `assertNotEqual`
                      ((max_digit_value * max_digit_value) + 1))
        ]

compareTests : Test
compareTests =
    suite "compare testsuite"
        [ equals (Data.Integer.compare (fromInt 1234567890) (fromInt 3)) GT
        , equals (Data.Integer.compare (fromInt 3) (fromInt 3)) EQ
        , equals (Data.Integer.compare (fromInt 1) (fromInt 3)) LT
        , equals (gt (fromInt 1234567890) (fromInt 3)) True
        , equals (gt (fromInt 3) (fromInt 3)) False
        , equals (gt (fromInt 1) (fromInt 3)) False
        , equals (gte (fromInt 1234567890) (fromInt 3)) True
        , equals (gte (fromInt 3) (fromInt 3)) True
        , equals (gte (fromInt 1) (fromInt 3)) False
        , equals (eq (fromInt 1234567890) (fromInt 3)) False
        , equals (eq (fromInt 3) (fromInt 3)) True
        , equals (lt (fromInt 1234567890) (fromInt 3)) False
        , equals (lt (fromInt 3) (fromInt 3)) False
        , equals (lt (fromInt 1) (fromInt 3)) True
        , equals (lte (fromInt 1234567890) (fromInt 3)) False
        , equals (lte (fromInt 3) (fromInt 3)) True
        , equals (lte (fromInt 1) (fromInt 3)) True
        ]

allTests : Test
allTests =
    suite "All tests"
        [ addTests
        , negateTests
        , subTests
        , mulTests
        , toStringTests
        , fromStringTests
        , minMaxTests
        , compareTests
        , divmodTests
        , max_digit_valueTests
        , Check.Test.evidenceToTest (quickCheck qcAdd)
        , Check.Test.evidenceToTest (quickCheck qcNegate)
        , Check.Test.evidenceToTest (quickCheck qcSub)
        , Check.Test.evidenceToTest (quickCheck qcMul)
        , Check.Test.evidenceToTest (quickCheck qcDivMod)
        , Check.Test.evidenceToTest (quickCheck qcAbs)
        , Check.Test.evidenceToTest (quickCheck qcSign)
        ]


main =
  runSuiteHtml allTests
