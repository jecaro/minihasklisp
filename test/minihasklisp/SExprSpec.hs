module SExprSpec (spec) where

import Eval (Error (NotBound))
import ParseAndEval (Error (..), parseAndEval)
import Parser (Parser (..))
import SExpr (SExpr (..), parseSExpr)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
    describe "SExpr" $ do
        it "simple list" $ do
            parse "(1 2 3)"
                `shouldBe` Just
                    ( SExpr
                        [ Atom "1"
                        , Atom "2"
                        , Atom "3"
                        , Atom "()"
                        ]
                    , ""
                    )

        it "nested list" $ do
            parse "(1 (1 2 (1 2 3)) 3)"
                `shouldBe` Just
                    ( SExpr
                        [ Atom "1"
                        , SExpr
                            [ Atom "1"
                            , Atom "2"
                            , SExpr [Atom "1", Atom "2", Atom "3", Atom "()"]
                            , Atom "()"
                            ]
                        , Atom "3"
                        , Atom "()"
                        ]
                    , ""
                    )

        it "syntaxic sugar" $ do
            parse "'()" `shouldBe` Just (SExpr [Atom "quote", Atom "()", Atom "()"], "")
            parse "'foo" `shouldBe` Just (SExpr [Atom "quote", Atom "foo", Atom "()"], "")
            parse "'(1 2 3)"
                `shouldBe` Just
                    ( SExpr
                        [ Atom "quote"
                        , SExpr
                            [ Atom "1"
                            , Atom "2"
                            , Atom "3"
                            , Atom "()"
                            ]
                        , Atom "()"
                        ]
                    , ""
                    )

        it "parse pair" $ do
            parse "(1 . ())" `shouldBe` Just (SExpr [Atom "1", Atom "()"], "")
            parse "(1 . (2 . ()))"
                `shouldBe` Just (SExpr [Atom "1", Atom "2", Atom "()"], "")
            parse "'((a . b) . ((foo . bar) . ((5 . 6) . ())))"
                `shouldBe` Just
                    ( SExpr
                        [ Atom "quote"
                        , SExpr
                            [ SExpr
                                [Atom "a", Atom "b"]
                            , SExpr [Atom "foo", Atom "bar"]
                            , SExpr [Atom "5", Atom "6"]
                            , Atom "()"
                            ]
                        , Atom "()"
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
                        [Atom "1", Atom "2", Atom "3", Atom "()"]
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
                        , SExpr [Atom "a", Atom "b", Atom "()"]
                        , SExpr [Atom "+", Atom "a", Atom "b", Atom "()"]
                        , Atom "()"
                        ]
                    , []
                    )
            parseAndEval "((lambda (a b) (+ a b)) 1 2)" []
                `shouldBe` Right (Atom "3", [])

        it "define" $ do
            parseAndEval "(foo)" [] `shouldBe` Left (ErEval (NotBound "foo"))
            parseAndEval "(define foo 42)" []
                `shouldBe` Right (Atom "42", [("foo", Atom "42")])
            parseAndEval "(define foo 42) foo" []
                `shouldBe` Right (Atom "42", [("foo", Atom "42")])
            let addBody =
                    SExpr
                        [ Atom "lambda"
                        , SExpr [Atom "a", Atom "b", Atom "()"]
                        , SExpr [Atom "+", Atom "a", Atom "b", Atom "()"]
                        , Atom "()"
                        ]
            parseAndEval "(define add (lambda (a b) (+ a b)))" []
                `shouldBe` Right (addBody, [("add", addBody)])
            parseAndEval "(define add (lambda (a b) (+ a b))) (add 1 3)" []
                `shouldBe` Right (Atom "4", [("add", addBody)])
            let subBody =
                    SExpr
                        [ Atom "lambda"
                        , SExpr [Atom "a", Atom "b", Atom "()"]
                        , SExpr [Atom "-", Atom "a", Atom "b", Atom "()"]
                        , Atom "()"
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
    describe "Files" $ do
        it "fact" $ do
            evalFile "test/minihasklisp/files/fact.scm" `shouldReturn` Right (Atom "3628800")
        it "fib" $ do
            evalFile "test/minihasklisp/files/fib.scm" `shouldReturn` Right (Atom "55")
        it "sort" $ do
            evalFile "test/minihasklisp/files/sort.scm"
                `shouldReturn` Right
                    ( SExpr
                        [ Atom "1"
                        , Atom "2"
                        , Atom "3"
                        , Atom "4"
                        , Atom "5"
                        , Atom "6"
                        , Atom "7"
                        , Atom "8"
                        , Atom "9"
                        , Atom "10"
                        , Atom "11"
                        , Atom "12"
                        , Atom "13"
                        , Atom "14"
                        , Atom "15"
                        , Atom "16"
                        , Atom "17"
                        , Atom "18"
                        , Atom "19"
                        , Atom "20"
                        , Atom "21"
                        , Atom "22"
                        , Atom "23"
                        , Atom "24"
                        , Atom "25"
                        , Atom "26"
                        , Atom "27"
                        , Atom "28"
                        , Atom "29"
                        , Atom "30"
                        , Atom "31"
                        , Atom "32"
                        , Atom "33"
                        , Atom "34"
                        , Atom "35"
                        , Atom "36"
                        , Atom "37"
                        , Atom "38"
                        , Atom "39"
                        , Atom "40"
                        , Atom "41"
                        , Atom "42"
                        , Atom "()"
                        ]
                    )
        it "qsort3" $ do
            evalFile "test/minihasklisp/files/qsort3.scm"
                `shouldReturn` Right
                    ( SExpr
                        [ Atom "1"
                        , Atom "2"
                        , Atom "3"
                        , Atom "4"
                        , Atom "5"
                        , Atom "6"
                        , Atom "7"
                        , Atom "8"
                        , Atom "9"
                        , Atom "()"
                        ]
                    )
  where
    parse = runParser parseSExpr
    evalFile file = do
        content <- readFile file
        pure $ fst <$> parseAndEval content []
