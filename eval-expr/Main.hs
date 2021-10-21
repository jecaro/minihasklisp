import EvalExpr.Expression
import Parser

main :: IO ()
main = interact (unlines . fmap interpret . lines)

interpret :: String -> String
interpret = render . runParser parseExpression
  where
    render Nothing = "Unable to parse expression"
    render (Just (x, _)) = "= " <> show (eval x)
