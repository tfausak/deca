import qualified Deca
import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ "Deca" ~:
    [ "abs" ~:
        [ Deca.abs (Deca.deca 0 0) ~?= Deca.Deca 0 0
        , Deca.abs (Deca.deca 1 0) ~?= Deca.Deca 1 0
        , Deca.abs (Deca.deca (-1) 0) ~?= Deca.Deca 1 0
        ]
    , "add" ~:
        [ Deca.add 1 2 ~?= Deca.Deca 3 0
        , Deca.add 1 20 ~?= Deca.Deca 21 0
        , Deca.add 10 2 ~?= Deca.Deca 12 0
        , Deca.add 5 5 ~?= Deca.Deca 1 1
        ]
    , "deca" ~:
        [ Deca.deca 0 0 ~?= Deca.Deca 0 0
        ]
    , "denormalize" ~:
        [ Deca.denormalize 0 0 ~?= (Deca.Deca 0 0, Deca.Deca 0 0)
        , Deca.denormalize 1 0 ~?= (Deca.Deca 1 0, Deca.Deca 0 0)
        , Deca.denormalize 0 1 ~?= (Deca.Deca 0 0, Deca.Deca 1 0)
        , Deca.denormalize 10 2 ~?= (Deca.Deca 10 0, Deca.Deca 2 0)
        , Deca.denormalize 1 20 ~?= (Deca.Deca 1 0, Deca.Deca 20 0)
        ]
    , "fromInteger" ~:
        [ Deca.fromInteger 0 ~?= Deca.Deca 0 0
        , Deca.fromInteger 1 ~?= Deca.Deca 1 0
        , Deca.fromInteger 10 ~?= Deca.Deca 1 1
        ]
    , "multiply" ~:
        [ Deca.multiply 2 3 ~?= Deca.Deca 6 0
        , Deca.multiply 2 30 ~?= Deca.Deca 6 1
        , Deca.multiply 20 3 ~?= Deca.Deca 6 1
        , Deca.multiply 2 5 ~?= Deca.Deca 1 1
        , Deca.multiply 20 30 ~?= Deca.Deca 6 2
        ]
    , "negate" ~:
        [ Deca.negate (Deca.deca 0 0) ~?= Deca.Deca 0 0
        , Deca.negate (Deca.deca 1 0) ~?= Deca.Deca (-1) 0
        , Deca.negate (Deca.deca (-1) 0) ~?= Deca.Deca 1 0
        ]
    , "normalize" ~:
        [ Deca.normalize (Deca.Deca 0 0) ~?= Deca.Deca 0 0
        , Deca.normalize (Deca.Deca 0 1) ~?= Deca.Deca 0 0
        , Deca.normalize (Deca.Deca 0 (-1)) ~?= Deca.Deca 0 0
        , Deca.normalize (Deca.Deca 1 0) ~?= Deca.Deca 1 0
        , Deca.normalize (Deca.Deca 10 0) ~?= Deca.Deca 1 1
        , Deca.normalize (Deca.Deca 12 0) ~?= Deca.Deca 12 0
        , Deca.normalize (Deca.Deca 10 1) ~?= Deca.Deca 1 2
        , Deca.normalize (Deca.Deca 10 (-1)) ~?= Deca.Deca 1 0
        ]
    , "signum" ~:
        [ Deca.signum (Deca.deca 0 0) ~?= Deca.Deca 0 0
        , Deca.signum (Deca.deca 2 0) ~?= Deca.Deca 1 0
        , Deca.signum (Deca.deca (-2) 0) ~?= Deca.Deca (-1) 0
        ]
    ]
