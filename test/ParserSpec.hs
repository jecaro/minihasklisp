module ParserSpec (spec)
where

import Test.Hspec

import Control.Applicative
import Parser

spec :: Spec
spec = do
    describe "Parser" $ do
        it "parseChar" $ do
            runParser (parseChar 'a') "abcd" `shouldBe` Just ('a', "bcd")
            runParser (parseChar 'z') "abcd" `shouldBe` Nothing
            runParser (parseChar 'a') "aaaa" `shouldBe` Just ('a', "aaa")

        it "parseAnyChar" $ do
            runParser (parseAnyChar "bca") "abcd" `shouldBe` Just ('a', "bcd")
            runParser (parseAnyChar "xyz") "abcd" `shouldBe` Nothing
            runParser (parseAnyChar "bca") "cdef" `shouldBe` Just ('c', "def")

        it "parseOr" $ do
            runParser (parseChar 'a' <|> parseChar 'b') "abcd"
                `shouldBe` Just ('a', "bcd")
            runParser (parseChar 'a' <|> parseChar 'b') "bcda"
                `shouldBe` Just ('b', "cda")
            runParser (parseChar 'a' <|> parseChar 'b') "xyz"
                `shouldBe` Nothing

        it "parseAnd" $ do
            runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
                `shouldBe` Just (('a', 'b'), "cd")
            runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda"
                `shouldBe` Nothing
            runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"
                `shouldBe` Nothing

        it "parseAndWith" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd"
                `shouldBe` Just ("ab", "cd")

        it "parseMany" $ do
            runParser (many (parseChar ' ')) "    foobar"
                `shouldBe` Just ("    ", "foobar")
            runParser (many (parseChar ' ')) "foobar    "
                `shouldBe` Just ("", "foobar    ")

        it "parseSome" $ do
            runParser (some (parseAnyChar ['0' .. '9'])) "42foobar"
                `shouldBe` Just ("42", "foobar")
            runParser (some (parseAnyChar ['0' .. '9'])) "foobar42"
                `shouldBe` Nothing

        it "parseUInt" $ do
            runParser parseUInt "42foobar" `shouldBe` Just (42, "foobar")
            runParser parseUInt "-42foobar" `shouldBe` Nothing

        it "parseInt" $ do
            runParser parseInt "42foobar" `shouldBe` Just (42, "foobar")
            runParser parseInt "-42foobar" `shouldBe` Just (-42, "foobar")

        it "parseDouble" $ do
            runParser parseDouble "42foobar" `shouldBe` Just (42, "foobar")
            runParser parseDouble "-42foobar" `shouldBe` Just (-42, "foobar")
            runParser parseDouble "42.13foobar" `shouldBe` Just (42.13, "foobar")
            runParser parseDouble "-42-foobar" `shouldBe` Just (-42, "-foobar")

        it "parseTuple" $ do
            runParser (parseTuple parseInt) "(123,456) foo bar "
                `shouldBe` Just ((123, 456), " foo bar ")

        it "parseTuple'" $ do
            runParser (parseTuple' parseInt) "(123,456) foo bar "
                `shouldBe` Just ((123, 456), " foo bar ")
