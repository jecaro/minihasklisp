import Test.Hspec

import Parser

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        it "parseChar" $ do
            runParser (parseChar 'a') "abcd" `shouldBe` Just ('a', "bcd")
            runParser (parseChar 'z') "abcd" `shouldBe` Nothing
            runParser (parseChar 'a') "aaaa" `shouldBe` Just ('a', "aaa")

        it "parseAnyChar" $ do
            runParser (parseAnyChar "bca") "abcd" `shouldBe` Just ('a', "bcd")
            runParser (parseAnyChar "xyz") "abcd" `shouldBe` Nothing
            runParser (parseAnyChar "bca") "cdef" `shouldBe` Just ('c', "def")

        it "parseAnyChar'" $ do
            runParser (parseAnyChar' "bca") "abcd" `shouldBe` Just ('a', "bcd")
            runParser (parseAnyChar' "xyz") "abcd" `shouldBe` Nothing
            runParser (parseAnyChar' "bca") "cdef" `shouldBe` Just ('c', "def")

        it "parseOr" $ do
            runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
                `shouldBe` Just ('a', "bcd")
            runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda"
                `shouldBe` Just ('b', "cda")
            runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz"
                `shouldBe` Nothing

        it "parseAnd" $ do
            runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
                `shouldBe` Just (('a', 'b'), "cd")
            runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda"
                `shouldBe` Nothing
            runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"
                `shouldBe` Nothing

        it "parseMany" $ do
            runParser (parseMany (parseChar ' ')) "    foobar"
                `shouldBe` Just ("    ", "foobar")
            runParser (parseMany (parseChar ' ')) "foobar    "
                `shouldBe` Just ("", "foobar    ")

        it "parseSome" $ do
            runParser (parseSome (parseAnyChar ['0' .. '9'])) "42foobar"
                `shouldBe` Just ("42", "foobar")
            runParser (parseSome (parseAnyChar ['0' .. '9'])) "foobar42"
                `shouldBe` Nothing

        it "parseUInt" $ do
            runParser parseUInt "42foobar" `shouldBe` Just (42, "foobar")
            runParser parseUInt "-42foobar" `shouldBe` Nothing

        it "parseInt" $ do
            runParser parseInt "42foobar" `shouldBe` Just (42, "foobar")
            runParser parseInt "-42foobar" `shouldBe` Just (-42, "foobar")

        it "parseTuple" $ do
            runParser (parseTuple parseInt) "(123,456) foo bar "
                `shouldBe` Just ((123, 456), " foo bar ")
