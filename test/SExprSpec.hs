module SExprSpec (spec) where

import Test.Hspec

import Parser
import SExpr

spec :: Spec
spec = do
    describe "SExpr" $ do
        it "parseAtom" $ do
            runParser parseAtom "()" `shouldBe` Just (SAtom Nil, "")
            runParser parseAtom "1234" `shouldBe` Just (SAtom (Atom "1234"), "")
        it "parsePair" $ do
            runParser
                parsePair
                "(1234 . abcd)"
                `shouldBe` Just (Pair (SAtom (Atom "1234")) (SAtom (Atom "abcd")), "")
            runParser
                parsePair
                "((a . b) . ((foo . bar) . ((5 . 6) . ())))"
                `shouldBe` Just
                    ( Pair
                        (Pair (SAtom (Atom "a")) (SAtom (Atom "b")))
                        ( Pair
                            (Pair (SAtom (Atom "foo")) (SAtom (Atom "bar")))
                            ( Pair
                                (Pair (SAtom (Atom "5")) (SAtom (Atom "6")))
                                (SAtom Nil)
                            )
                        )
                    , ""
                    )
        it "parseSExpr" $ do
            runParser
                parseSExpr
                "'((a . b) . ((foo . bar) . ((5 . 6) . ())))"
                `shouldBe` Just
                    ( Pair
                        (Pair (SAtom (Atom "a")) (SAtom (Atom "b")))
                        ( Pair
                            (Pair (SAtom (Atom "foo")) (SAtom (Atom "bar")))
                            ( Pair
                                (Pair (SAtom (Atom "5")) (SAtom (Atom "6")))
                                (SAtom Nil)
                            )
                        )
                    , ""
                    )
            runParser
                parseSExpr
                "'(1 2 3)"
                `shouldBe` Just
                    ( Pair
                        (SAtom (Atom "1"))
                        (Pair (SAtom (Atom "2")) (Pair (SAtom (Atom "3")) (SAtom Nil)))
                    , ""
                    )
