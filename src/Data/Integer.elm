module Data.Integer exposing
    ( Integer
    , Sign(Positive, Negative)
    , sign
    , max_digit_value
    , fromInt
    , fromString
    , toString
    , add
    , sub
    , negate
    , mul
    , divmod
    , unsafeDivmod
    , abs
    , compare
    , gt
    , gte
    , lt
    , lte
    , eq
    , neq
    , max
    , min
    , zero
    , one
    , minusOne
    )


{-| Infinite digits integers
# The datatype
@docs Integer
@docs Sign

# From/To
@docs fromInt
@docs fromString
@docs toString

# Common operations
@docs add
@docs sub
@docs negate
@docs mul
@docs divmod
@docs unsafeDivmod
@docs abs
@docs sign

# Comparison
@docs compare
@docs gt
@docs gte
@docs lt
@docs lte
@docs eq
@docs neq
@docs max
@docs min

# Common numbers
@docs zero
@docs one
@docs minusOne

# Internals
@docs max_digit_value

-}

import String
import Maybe exposing (Maybe)
import Result exposing (Result)
import Char
import Basics
import Debug

{-| The sign of the integer -}
type Sign
    = Positive
    | Negative


type alias Digit = Int


{- From smallest to largest digit, all the digits are positive, no leading zeros -}
type Magnitude = Magnitude (List Digit)


type MagnitudeNotNormalised = MagnitudeNotNormalised (List Digit)


{-| Integer type -}
type Integer = Integer (Sign, Magnitude)


type IntegerNotNormalised = IntegerNotNormalised (Sign, MagnitudeNotNormalised)


{-| Enough to hold digit * digit without overflowing to double -}
max_digit_value : Int
max_digit_value = 1000000


{-| Makes an Integer from an Int -}
fromInt : Int -> Integer
fromInt x =
    let sign = if x < 0 then Negative else Positive in
    normalise <| IntegerNotNormalised (sign, MagnitudeNotNormalised [Basics.abs x])


{-| Makes an Integer from a String -}
fromString : String -> Maybe Integer
fromString x =
    case String.toList x of
        [] -> Just (fromInt 0)
        '-'::xs ->
            case fromString' xs of
                Nothing -> Nothing
                (Just a) -> Just (Integer (Negative, a))
        '+'::xs ->
            case fromString' xs of
                Nothing -> Nothing
                (Just a) -> Just (Integer (Positive, a))
        xs ->
            case fromString' xs of
                Nothing -> Nothing
                (Just a) -> Just (Integer (Positive, a))


fromString' : List Char -> Maybe Magnitude
fromString' x =
    if not <| List.all Char.isDigit x
    then Nothing
    else
        let rev_digits = List.reverse x
            rev_group_digits = groups 6 rev_digits
            group_digits = List.map List.reverse rev_group_digits
            group_strings = List.map String.fromList group_digits
            group_result_ints = List.map String.toInt group_strings
        in
        let result_to_maybe x =
            case x of
                Ok a -> Just a
                Err _ -> Nothing
        in
        let group_maybe_ints = List.map result_to_maybe group_result_ints in 
        let gen_res x =
            case x of
                [] -> Just []
                Nothing :: xs -> Nothing
                (Just b) :: bx ->
                    case gen_res bx of
                        Nothing -> Nothing
                        Just xxs -> Just (b::xxs)
        in
        case gen_res group_maybe_ints of
            Just x -> Just (Magnitude x)
            Nothing -> Nothing


groups : Int -> List a -> List (List a)
groups n x =
    let head = List.take n x
        tail = List.drop n x in
    if tail == []
    then [head]
    else head :: groups n tail


type MagnitudePair = MagnitudePair (List (Digit, Digit))


sameSize : Magnitude -> Magnitude -> MagnitudePair
sameSize (Magnitude a) (Magnitude b) =
    let sameSize' a b =
        case (a, b) of
            ([], []) -> []
            (a::xa, b::xb) ->
                (a, b) :: sameSize' xa xb
            (a::xa, []) ->
                (a, 0) :: sameSize' xa []
            ([], b::xb) ->
                (0, b) :: sameSize' [] xb
    in
    MagnitudePair (sameSize' a b)


sameSize' : MagnitudeNotNormalised -> MagnitudeNotNormalised -> MagnitudePair
sameSize' (MagnitudeNotNormalised a) (MagnitudeNotNormalised b) =
    let sameSize'' a b =
        case (a, b) of
            ([], []) -> []
            (a::xa, b::xb) ->
                (a, b) :: sameSize'' xa xb
            (a::xa, []) ->
                (a, 0) :: sameSize'' xa []
            ([], b::xb) ->
                (0, b) :: sameSize'' [] xb
    in
    MagnitudePair (sameSize'' a b)


normalise : IntegerNotNormalised -> Integer
normalise (IntegerNotNormalised (sx, x)) =
    let nmagnitude = normaliseMagnitude x in
    let is_negative_magnitude (Magnitude x) =
        case x of
            [] -> False
            [d] -> d < 0
            x::xs -> is_negative_magnitude (Magnitude xs)
    in
    let reverse_magnitude (Magnitude xs) =
        MagnitudeNotNormalised (List.map (\x -> 0-x) xs)
    in
    let reverse_sign s =
        case s of
            Positive -> Negative
            Negative -> Positive
    in
    if is_negative_magnitude nmagnitude
    then normalise (IntegerNotNormalised (reverse_sign sx, reverse_magnitude nmagnitude))
    else Integer (sx, nmagnitude)


normaliseDigit : Int -> (Int, Digit)
normaliseDigit d =
    if d < 0
    then let (carry, d') = normaliseDigit (d + max_digit_value) in
        (carry - 1, d')
    else (d // max_digit_value, d `rem` max_digit_value)


normaliseDigitList : List Int -> List Digit
normaliseDigitList x =
    case x of
        [] -> []
        d :: [] ->
            let (c, d') = normaliseDigit d in
            if c /= 0
            then [d', c]
            else [d']
        d :: d2 :: xs ->
            let (c, d') = normaliseDigit d in
            d' :: normaliseDigitList (d2 + c :: xs)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile f x =
    case x of
        [] -> []
        a :: xs ->
            if f a
            then dropWhile f xs
            else a :: xs
            

dropZeroes : List Digit -> List Digit
dropZeroes x =
    let rev_list = List.reverse x
        no_zeros = dropWhile (\x -> x == 0) rev_list in
    List.reverse no_zeros


normaliseMagnitude : MagnitudeNotNormalised -> Magnitude
normaliseMagnitude (MagnitudeNotNormalised x) =
    Magnitude (dropZeroes (normaliseDigitList x))


toPositiveSign : Integer -> IntegerNotNormalised
toPositiveSign (Integer (s, Magnitude m)) =
    let reverse_magnitude (Magnitude xs) =
        MagnitudeNotNormalised (List.map (\x -> -x) xs)
    in
    case s of
        Positive -> IntegerNotNormalised (s, MagnitudeNotNormalised m)
        Negative -> IntegerNotNormalised (Positive, reverse_magnitude (Magnitude m))


{-| Adds two Integers -}
add : Integer -> Integer -> Integer
add a b =
    let (IntegerNotNormalised (_, ma)) = toPositiveSign a
        (IntegerNotNormalised (_, mb)) = toPositiveSign b
        (MagnitudePair (p)) = sameSize' ma mb
        added = List.map (\(x, y) -> x+y) p
    in
    normalise (IntegerNotNormalised (Positive, MagnitudeNotNormalised added))

    
{-| Changes the sign of an Integer -}
negate : Integer -> Integer
negate (Integer (s, m)) =
    let newsign = case s of
                  Positive -> Negative
                  Negative -> Positive
    in
    normalise (toPositiveSign (Integer (newsign, m)))


{-| Absolute value -}
abs : Integer -> Integer
abs (Integer (s, m)) = Integer (Positive, m)

        
{-| Substracts the second Integer from the first -}
sub : Integer -> Integer -> Integer
sub a b = add a (negate b)


{-| Multiplies two Integers -}
mul : Integer -> Integer -> Integer
mul (Integer (s1, m1)) (Integer (s2, m2)) =
    let sign = case (s1, s2) of
                    (Positive, Positive) -> Positive
                    (Negative, Negative) -> Positive
                    _ -> Negative
    in
    Integer (sign, (mul_magnitudes m1 m2))


mul_magnitudes : Magnitude -> Magnitude -> Magnitude
mul_magnitudes (Magnitude m1) (Magnitude m2) =
    case m1 of
        [] ->
            Magnitude []
        [m] ->
            mul_single_digit (Magnitude m2) m
        m :: mx ->
            let accum = mul_single_digit (Magnitude m2) m
                (Magnitude rest) = mul_magnitudes (Magnitude mx) (Magnitude m2)
                i1 = (Integer (Positive, accum))
                i2 = (Integer (Positive, (Magnitude (0 :: rest))))
                (Integer (_, result)) = i1 `add` i2
            in
            result
                

mul_single_digit : Magnitude -> Digit -> Magnitude
mul_single_digit (Magnitude m) d =
    normaliseMagnitude (MagnitudeNotNormalised (List.map (\x -> d * x) m))


{-| Compares two Integers -}
compare : Integer -> Integer -> Order
compare (Integer (sa, a)) (Integer (sb, b)) =
    let invert_order x =
        case x of
            LT -> GT
            EQ -> EQ
            GT -> LT
    in
    case (sa, sb) of
        (Positive, Negative) -> GT
        (Negative, Positive) -> LT
        _ -> 
            let ss = sameSize a b
                rss = reverseMagnitudePair ss
                cr = compareMagnitude rss in
            if sa == Positive
            then cr
            else invert_order cr


{-| Equals -}
eq : Integer -> Integer -> Bool
eq a b =
    case compare a b of
        EQ -> True
        _ -> False


{-| Not equals -}
neq : Integer -> Integer -> Bool
neq a b = not (eq a b)


{-| Less than -}
lt : Integer -> Integer -> Bool
lt a b =
    case compare a b of
        LT -> True
        _ -> False


{-| Greater than -}
gt : Integer -> Integer -> Bool
gt a b =
    case compare a b of
        GT -> True
        _ -> False


{-| Greater than or equals -}
gte : Integer -> Integer -> Bool
gte a b =
    case compare a b of
        GT -> True
        EQ -> True
        _ -> False


{-| Less than or equals -}
lte : Integer -> Integer -> Bool
lte a b =
    case compare a b of
        LT -> True
        EQ -> True
        _ -> False


{-| Returns the largest of two Integers -}
max : Integer -> Integer -> Integer
max a b =
    case compare a b of
        GT -> a
        EQ -> a
        LT -> b


{-| Returns the smallest of two Integers -}
min : Integer -> Integer -> Integer
min a b =
    case compare a b of
        LT -> a
        EQ -> a
        GT -> b


type MagnitudePairReverseOrder = MagnitudePairReverseOrder (List (Digit, Digit))


reverseMagnitudePair : MagnitudePair -> MagnitudePairReverseOrder
reverseMagnitudePair (MagnitudePair x) = MagnitudePairReverseOrder <| List.reverse x


compareMagnitude : MagnitudePairReverseOrder -> Order
compareMagnitude (MagnitudePairReverseOrder m) =
    case m of
        [] -> EQ
        (a, b) :: xs ->
            if a == b
            then compareMagnitude (MagnitudePairReverseOrder xs)
            else Basics.compare a b


zeroes : Int -> String
zeroes n =
    String.repeat n "0"


fillZeroes : Digit -> String
fillZeroes d =
    let d_s = Basics.toString d in
    let len = String.length d_s in
    zeroes (6 - len) ++ d_s


revmagnitudeToString : List Digit -> String
revmagnitudeToString m =
    case m of
        [] ->
            "0"
        [x] ->
            Basics.toString x
        x :: xs ->
            (Basics.toString x) ++ String.concat (List.map fillZeroes xs)
    

{-| Converts the Integer to a String -}
toString : Integer -> String
toString (Integer (s, Magnitude m)) =
    let sign = if s == Positive then "" else "-" in
    sign ++ revmagnitudeToString (List.reverse m)


range : Int -> Int -> List Int
range a b =
    if a == b
    then [a]
    else a :: range (a+1) b


dividers : List Integer
dividers =
    let log = Basics.logBase 2 (Basics.toFloat max_digit_value)
        log_i = (Basics.truncate log) + 1
        exp_values = List.reverse (range 0 log_i)
        int_values = List.map (\x -> 2 ^ x) exp_values
    in
    List.map fromInt int_values


pad_digits : Int -> Integer
pad_digits n =
    if n == 0
    then fromInt 1
    else (pad_digits (n-1)) `mul` (fromInt max_digit_value)


divmod_digit : Integer -> List Integer -> Integer -> Integer -> (Integer, Integer)
divmod_digit padding to_test a b =
    case to_test of
        [] ->
            (fromInt 0, a)
        x :: xs ->
            let candidate = x `mul` b `mul` padding
                (newdiv, newmod) = if candidate `lte` a
                                   then (x `mul` padding, a `sub`candidate)
                                   else (fromInt 0, a)
                (restdiv, restmod) = divmod_digit padding xs newmod b
            in
            (newdiv `add` restdiv, restmod)


divmod' : Int -> Integer -> Integer -> (Integer, Integer)
divmod' n a b =
    if n == 0
    then divmod_digit (pad_digits n) dividers a b
    else
        let (cdiv, cmod) = divmod_digit (pad_digits n) dividers a b
            (rdiv, rmod) = divmod' (n-1) cmod b
        in
            (cdiv `add` rdiv, rmod)


{-| Division and modulus -}
divmod : Integer -> Integer -> Maybe (Integer, Integer)
divmod a b =
    if b `eq` zero
    then Nothing
    else
        let (Integer (s1, Magnitude m1)) = a
            (Integer (s2, Magnitude m2)) = b
            cand_l = (List.length m1) - (List.length m2) + 1
            l = if cand_l < 0 then 0 else cand_l
            sign = case (s1, s2) of
                        (Positive, Positive) -> Positive
                        (Negative, Negative) -> Positive
                        _ -> Negative
            (Integer (_, d), Integer (_, m)) = divmod' l (abs a) (abs b)
        in
        Just (Integer (sign, d), Integer (s1, m))


{-| divmod that returns the pair of values, or crashes if the divisor is zero -}
unsafeDivmod : Integer -> Integer -> (Integer, Integer)
unsafeDivmod a b =
    let v = divmod a b in
    case v of
        Just r -> r
        Nothing -> Debug.crash "Divide by zero"


{-| Get the sign of the integer -}
sign : Integer -> Sign
sign (Integer (x, _)) = x


{-| Number 0 -}
zero : Integer
zero = fromInt 0


{-| Number 1 -}
one : Integer
one = fromInt 1


{-| Number -1 -}
minusOne : Integer
minusOne = fromInt -1
