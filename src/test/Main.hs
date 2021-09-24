import Deca
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ "Deca" ~:
    [ "denormalize" ~:
        [ denormalize 0 0 ~?= (Deca 0 0, Deca 0 0)
        , denormalize 1 0 ~?= (Deca 1 0, Deca 0 0)
        , denormalize 0 1 ~?= (Deca 0 0, Deca 1 0)
        , denormalize 10 2 ~?= (Deca 10 0, Deca 2 0)
        , denormalize 1 20 ~?= (Deca 1 0, Deca 20 0)
        ]
    , "deca" ~:
        [ deca 0 0 ~?= Deca 0 0
        ]
    , "normalize" ~:
        [ normalize (Deca 0 0) ~?= Deca 0 0
        , normalize (Deca 0 1) ~?= Deca 0 0
        , normalize (Deca 0 (-1)) ~?= Deca 0 0
        , normalize (Deca 1 0) ~?= Deca 1 0
        , normalize (Deca 10 0) ~?= Deca 1 1
        , normalize (Deca 12 0) ~?= Deca 12 0
        , normalize (Deca 10 1) ~?= Deca 1 2
        , normalize (Deca 10 (-1)) ~?= Deca 1 0
        ]
    , "Num" ~:
        [ "abs" ~:
            [ abs (deca 0 0) ~?= Deca 0 0
            , abs (deca 1 0) ~?= Deca 1 0
            , abs (deca (-1) 0) ~?= Deca 1 0
            ]
        , "fromInteger" ~:
            [ 0 ~?= Deca 0 0
            , 1 ~?= Deca 1 0
            , 10 ~?= Deca 1 1
            ]
        , "negate" ~:
            [ negate (deca 0 0) ~?= Deca 0 0
            , negate (deca 1 0) ~?= Deca (-1) 0
            , negate (deca (-1) 0) ~?= Deca 1 0
            ]
        , "signum" ~:
            [ signum (deca 0 0) ~?= Deca 0 0
            , signum (deca 2 0) ~?= Deca 1 0
            , signum (deca (-2) 0) ~?= Deca (-1) 0
            ]
        , "+" ~:
            [ 1 + 2 ~?= Deca 3 0
            , 1 + 20 ~?= Deca 21 0
            , 10 + 2 ~?= Deca 12 0
            , 5 + 5 ~?= Deca 1 1
            ]
        , "*" ~:
            [ 2 * 3 ~?= Deca 6 0
            , 2 * 30 ~?= Deca 6 1
            , 20 * 3 ~?= Deca 6 1
            , 2 * 5 ~?= Deca 1 1
            , 20 * 30 ~?= Deca 6 2
            ]
        ]
    ]
