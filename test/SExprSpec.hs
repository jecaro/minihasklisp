module SExprSpec (spec) where

import Hal.Eval (Error (NotBounded))
import Hal.ParseAndEval
import Hal.SExpr
import Parser
import Test.Hspec

spec :: Spec
spec = do
    describe "SExpr" $ do
        it "simple list" $ do
            parse "(1 2 3)" `shouldBe` Just (SExpr [Atom "1", Atom "2", Atom "3"], "")

        it "nested list" $ do
            parse "(1 (1 2 (1 2 3)) 3)"
                `shouldBe` Just
                    ( SExpr
                        [ Atom "1"
                        , SExpr
                            [ Atom "1"
                            , Atom "2"
                            , SExpr [Atom "1", Atom "2", Atom "3"]
                            ]
                        , Atom "3"
                        ]
                    , ""
                    )

        it "syntaxic sugar" $ do
            parse "'()" `shouldBe` Just (SExpr [Atom "quote", Atom "()"], "")
            parse "'foo" `shouldBe` Just (SExpr [Atom "quote", Atom "foo"], "")
            parse "'(1 2 3)"
                `shouldBe` Just
                    ( SExpr
                        [ Atom "quote"
                        , SExpr
                            [ Atom "1"
                            , Atom "2"
                            , Atom "3"
                            ]
                        ]
                    , ""
                    )

    describe "Eval" $ do
        it "cons" $ do
            parseAndEval "(cons 1 2)" []
                `shouldBe` Right (SExpr [Atom "1", Atom "2"], [])
            parseAndEval "(cons 1 (cons 2 (cons 3 '())))" []
                `shouldBe` Right
                    ( SExpr
                        [ Atom "1"
                        , SExpr [Atom "2", SExpr [Atom "3", Atom "()"]]
                        ]
                    , []
                    )

        it "cons" $ do
            parseAndEval "(car (cons 1 2))" [] `shouldBe` Right (Atom "1", [])

        it "cdr" $ do
            parseAndEval "(cdr (cons 1 2))" [] `shouldBe` Right (Atom "2", [])
            parseAndEval "(cdr '(1 2 3))" []
                `shouldBe` Right
                    ( SExpr
                        [ Atom "2"
                        , Atom "3"
                        , Atom "()"
                        ]
                    , []
                    )

        it "eq?" $ do
            parseAndEval "(eq? 1 1)" [] `shouldBe` Right (Atom "#t", [])
            parseAndEval "(eq? (+ 1 1) 2)" [] `shouldBe` Right (Atom "#t", [])
            parseAndEval "(eq? 'foo (car '(foo bar)))" []
                `shouldBe` Right (Atom "#t", [])
            parseAndEval "(eq? 'foo 'bar)" [] `shouldBe` Right (Atom "#f", [])
            parseAndEval "(eq? '() '())" [] `shouldBe` Right (Atom "#t", [])

        it "atom?" $ do
            parseAndEval "(atom? 'foo)" [] `shouldBe` Right (Atom "#t", [])
            parseAndEval "(atom? '(1 2 3))" [] `shouldBe` Right (Atom "#f", [])
            parseAndEval "(atom? '())" [] `shouldBe` Right (Atom "#t", [])

        it "arithmetic" $ do
            parseAndEval "(div (* 5 2) (- 3))" [] `shouldBe` Right (Atom "-3", [])
            parseAndEval "(< (* 2 2) 5)" [] `shouldBe` Right (Atom "#t", [])
            parseAndEval "(mod (+ 5 5) 3)" [] `shouldBe` Right (Atom "1", [])

        it "quote" $ do
            parseAndEval "(quote toto)" [] `shouldBe` Right (Atom "toto", [])
            parseAndEval "(quote (+ 1 2))" []
                `shouldBe` Right
                    ( SExpr
                        [ Atom "+"
                        , Atom "1"
                        , Atom "2"
                        , Atom "()"
                        ]
                    , []
                    )
            parseAndEval "'toto" [] `shouldBe` Right (Atom "toto", [])
            parseAndEval "'(+ 1 2)" []
                `shouldBe` Right
                    ( SExpr
                        [ Atom "+"
                        , Atom "1"
                        , Atom "2"
                        , Atom "()"
                        ]
                    , []
                    )

        it "lambda" $ do
            parseAndEval "(lambda (a b) (+ a b))" []
                `shouldBe` Right
                    ( SExpr
                        [ Atom "lambda"
                        , SExpr [Atom "a", Atom "b"]
                        , SExpr [Atom "+", Atom "a", Atom "b"]
                        ]
                    , []
                    )
            parseAndEval "((lambda (a b) (+ a b)) 1 2)" []
                `shouldBe` Right (Atom "3", [])

        it "define" $ do
            parseAndEval "(foo)" [] `shouldBe` Left (ErEval (NotBounded "foo"))
            parseAndEval "(define foo 42)" []
                `shouldBe` Right (Atom "42", [("foo", Atom "42")])
            parseAndEval "(define foo 42) foo" []
                `shouldBe` Right (Atom "42", [("foo", Atom "42")])
            let addBody =
                    SExpr
                        [ Atom "lambda"
                        , SExpr [Atom "a", Atom "b"]
                        , SExpr [Atom "+", Atom "a", Atom "b"]
                        ]
            parseAndEval "(define add (lambda (a b) (+ a b)))" []
                `shouldBe` Right (addBody, [("add", addBody)])
            parseAndEval "(define add (lambda (a b) (+ a b))) (add 1 3)" []
                `shouldBe` Right (Atom "4", [("add", addBody)])
            let subBody =
                    SExpr
                        [ Atom "lambda"
                        , SExpr [Atom "a", Atom "b"]
                        , SExpr [Atom "-", Atom "a", Atom "b"]
                        ]
            parseAndEval "(define (sub a b) (- a b))" []
                `shouldBe` Right (subBody, [("sub", subBody)])
            parseAndEval "(define (sub a b) (- a b)) (sub 3 1)" []
                `shouldBe` Right (Atom "2", [("sub", subBody)])

        it "let" $ do
            parseAndEval "(let ((a 2) (b (+ 1 2))) (+ a b))" []
                `shouldBe` Right (Atom "5", [("a", Atom "2"), ("b", Atom "3")])

        it "cond" $ do
            parseAndEval "(cond (#f 1) (#t (+ 1 1)))" []
                `shouldBe` Right (Atom "2", [])

            parseAndEval
                "(cond ((eq? 'foo (car '(foo bar))) 'here)\
                \ ((eq? 1 2) 'there) (#t 'nope))"
                []
                `shouldBe` Right (Atom "here", [])
  where
    parse = runParser parseSExpr
