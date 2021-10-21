module ExpressionSpec (spec) where

import Test.Hspec

import EvalExpr.Expression
import Parser

spec :: Spec
spec = do
    describe "Expression" $ do
        it "parseOperator" $ do
            runParser parseOperator "+bcd" `shouldBe` Just (Plus, "bcd")
            runParser parseOperator "-bcd" `shouldBe` Just (Minus, "bcd")
            runParser parseOperator "*bcd" `shouldBe` Just (Product, "bcd")
            runParser parseOperator "/bcd" `shouldBe` Just (Divide, "bcd")
            runParser parseOperator "^bcd" `shouldBe` Just (Power, "bcd")
            runParser parseOperator "abcd" `shouldBe` Nothing
        it "parseExpression" $ do
            runParser parseExpression "14.3+" `shouldBe` Just (Num 14.3, "+")
            runParser parseExpression "14.3+3*4" `shouldSatisfy` allConsumed
            runParser parseExpression "2 * 3 + 4" `shouldSatisfy` allConsumed
            runParser parseExpression "(0.345+ 5 )*( -2-1) / 3" `shouldSatisfy` allConsumed
        it "parse and eval" $ do
            evaluate "2+3*4" `shouldBe` Just 14
            evaluate "(0.345+ 5 )*( -2-1) / 3" `shouldBe` Just (-5.345)
            evaluate "0.345+ 5 *( -2-1) / 3" `shouldBe` Just (-4.655)
            evaluate "(1 + 2)^(3+ 1)" `shouldBe` Just 81
  where
    evaluate str = eval . fst <$> runParser parseExpression str
    allConsumed (Just (_, "")) = True
    allConsumed _ = False
