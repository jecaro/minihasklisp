import Test.Hspec

import Parser

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        it "parseChar" $ do
            parseChar 'a' "abcd" `shouldBe` Just ('a', "bcd")
            parseChar 'z' "abcd" `shouldBe` Nothing
            parseChar 'a' "aaaa" `shouldBe` Just ('a', "aaa")

        it "parseAnyChar" $ do
            parseAnyChar "bca" "abcd" `shouldBe` Just ('a', "bcd")
            parseAnyChar "xyz" "abcd" `shouldBe` Nothing
            parseAnyChar "bca" "cdef" `shouldBe` Just ('c', "def")

        it "parseAnyChar'" $ do
            parseAnyChar' "bca" "abcd" `shouldBe` Just ('a', "bcd")
            parseAnyChar' "xyz" "abcd" `shouldBe` Nothing
            parseAnyChar' "bca" "cdef" `shouldBe` Just ('c', "def")

        it "parseOr" $ do
            parseOr (parseChar 'a') (parseChar 'b') "abcd"
                `shouldBe` Just ('a', "bcd")
            parseOr (parseChar 'a') (parseChar 'b') "bcda"
                `shouldBe` Just ('b', "cda")
            parseOr (parseChar 'a') (parseChar 'b') "xyz"
                `shouldBe` Nothing

        it "parseAnd" $ do
            parseAnd (parseChar 'a') (parseChar 'b') "abcd"
                `shouldBe` Just (('a', 'b'), "cd")
            parseAnd (parseChar 'a') (parseChar 'b') "bcda"
                `shouldBe` Nothing
            parseAnd (parseChar 'a') (parseChar 'b') "acd"
                `shouldBe` Nothing

        it "parseMany" $ do
            parseMany (parseChar ' ') "    foobar"
                `shouldBe` Just ("    ", "foobar")
            parseMany (parseChar ' ') "foobar    "
                `shouldBe` Just ("", "foobar    ")

        it "parseSome" $ do
            parseSome (parseAnyChar ['0' .. '9']) "42foobar"
                `shouldBe` Just ("42", "foobar")
            parseSome (parseAnyChar ['0' .. '9']) "foobar42"
                `shouldBe` Nothing

        it "parseUInt" $ do
            parseUInt "42foobar" `shouldBe` Just (42, "foobar")
            parseUInt "-42foobar" `shouldBe` Nothing

        it "parseInt" $ do
            parseInt "42foobar" `shouldBe` Just (42, "foobar")
            parseInt "-42foobar" `shouldBe` Just (-42, "foobar")

        it "parseTuple" $ do
            parseTuple parseInt "(123,456) foo bar "
                `shouldBe` Just ((123, 456), " foo bar ")
