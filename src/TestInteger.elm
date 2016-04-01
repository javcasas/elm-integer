import Data.Integer
import List
import Text
import Graphics.Element exposing (show, flow, down, Element, leftAligned)

failures : List Graphics.Element.Element
failures = 
    let all_tests = Data.Integer.test ()
        failures = List.filter (\(x, y) -> y == False) all_tests
        texts = List.map (\(x, y) -> x) failures
    in
    List.map (\x -> leftAligned (Text.fromString x)) texts

main =
    let res = if failures == [] then "All Ok" else "Some failures"
    in
    flow down (failures ++ [(leftAligned (Text.fromString res))])
