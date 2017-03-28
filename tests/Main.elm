port module Main exposing (..)

import Check exposing (quickCheck, Claim, claim, that, is, for)
import Check.Producer exposing (int, tuple, rangeInt)
import Data.Integer exposing (..)
import Legacy.Check.Test as CheckTest
import Legacy.ElmTest as ElmTest exposing (..)
import Maybe exposing (Maybe)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


integer : Check.Producer.Producer Integer
integer =
    Check.Producer.map (\a -> fromInt a) int


addTests : Test
addTests =
    let
        one =
            fromInt 1

        two =
            fromInt 2

        three =
            fromInt 3
    in
        suite "Add testsuite"
            [ equals (add one two) three
            ]


qcAdd : Claim
qcAdd =
    Check.suite "Quickcheck Add"
        [ for (is (that (claim "Conmutative adding") (\( a, b ) -> add a b)) (\( a, b ) -> add b a)) (tuple ( integer, integer ))
        ]


negateTests : Test
negateTests =
    let
        one =
            fromInt 1
    in
        suite "Negate testsuite"
            [ equals (Data.Integer.negate (fromInt 1)) (fromInt -1)
            , equals (Data.Integer.negate (fromInt -1)) (fromInt 1)
            ]


qcNegate : Claim
qcNegate =
    Check.suite "Quickcheck Negate"
        [ for (is (that (claim "Double negate is noop") (\a -> (Data.Integer.negate (Data.Integer.negate a)))) (\a -> a)) integer
        ]


subTests : Test
subTests =
    let
        one =
            fromInt 1

        two =
            fromInt 2

        three =
            fromInt 3
    in
        suite "Sub testsuite"
            [ equals (sub three two) one
            ]


qcSub : Claim
qcSub =
    Check.suite "Quickcheck Sub"
        [ for (is (that (claim "Conmutative substract") (\( a, b ) -> sub a b)) (\( a, b ) -> (Data.Integer.negate (sub b a)))) (tuple ( integer, integer ))
        ]


mulTests : Test
mulTests =
    let
        six =
            fromInt 6

        two =
            fromInt 2

        three =
            fromInt 3
    in
        suite "Mul testsuite"
            [ equals (mul three two) six
            , equals (mul three (Data.Integer.negate two)) (Data.Integer.negate six)
            ]


qcMul : Claim
qcMul =
    Check.suite "Quickcheck Mul"
        [ for (is (that (claim "Conmutative multiplication") (\( a, b ) -> mul a b)) (\( a, b ) -> mul b a)) (tuple ( integer, integer ))
        ]


divmodTests : Test
divmodTests =
    suite "divmod testsuite"
        [ equals (divmod (fromInt 2000000001) (fromInt 2)) (Just ( fromInt 1000000000, fromInt 1 ))
        , equals (divmod (fromInt 2000000002) (fromInt 2)) (Just ( fromInt 1000000001, fromInt 0 ))
        , equals (divmod (fromInt 20) (fromInt 0)) Nothing
        ]


qcDivMod : Claim
qcDivMod =
    Check.suite "Quickcheck divmod"
        [ for
            (is
                (that (claim "divmod definition")
                    (\( a, b ) ->
                        let
                            ( c, r ) =
                                unsafeDivmod a b
                        in
                            add (mul c b) r
                    )
                )
                (\( a, b ) -> a)
            )
            (tuple ( integer, Check.Producer.filter (\x -> neq x zero) integer ))
        ]


qcAbs : Claim
qcAbs =
    Check.suite "Quickcheck abs"
        [ for (is (that (claim "abs is always positive") (\a -> gte (Data.Integer.abs a) (fromInt 0))) (\a -> True)) integer
        , for
            (is
                (that (claim "abs definition")
                    (\a ->
                        if gte a (fromInt 0) then
                            (eq (Data.Integer.abs a) a)
                        else
                            (eq (Data.Integer.abs a) (Data.Integer.negate a))
                    )
                )
                (\a -> True)
            )
            integer
        ]


qcSign : Claim
qcSign =
    Check.suite "Quickcheck sign"
        [ for
            (is
                (that (claim "sign definition")
                    (\a ->
                        if gte a zero then
                            Positive
                        else
                            Negative
                    )
                )
                (\a -> sign a)
            )
            integer
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
        [ defaultTest
            (assertNotEqual (max_digit_value * max_digit_value) ((max_digit_value * max_digit_value) + 1))
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
        , CheckTest.evidenceToTest (quickCheck qcAdd)
        , CheckTest.evidenceToTest (quickCheck qcNegate)
        , CheckTest.evidenceToTest (quickCheck qcSub)
        , CheckTest.evidenceToTest (quickCheck qcMul)
        , CheckTest.evidenceToTest (quickCheck qcDivMod)
        , CheckTest.evidenceToTest (quickCheck qcAbs)
        , CheckTest.evidenceToTest (quickCheck qcSign)
        ]


main =
    run emit allTests


port emit : ( String, Value ) -> Cmd msg
