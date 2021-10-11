import Eval
import Parser
import SExpr

main :: IO ()
main = interact (unlines . fmap interpret . lines)

interpret :: String -> String
interpret = render' . runParser parseSExpr
  where
    render' Nothing = "Unable to parse expression"
    render' (Just (x, _)) =
        unlines
            [ toPairs x
            , "= " <> toPairs' (snd (eval [] x))
            ]
