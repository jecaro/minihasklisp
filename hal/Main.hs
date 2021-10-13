import Data.Bifunctor
import Eval
import Parser
import SExpr

main :: IO ()
main = go []
  where
    go e = do
        line <- getLine
        let (e', r) = evaluate e (runParser parseSExpr line)
        putStrLn $ unlines r
        go e'

    evaluate e Nothing = (e, ["Unable to parse expression"])
    evaluate e (Just (s, _)) =
        ( e'
        ,
            [ "r = " <> toPairs' v
            , "e = " <> show (second toPairs' <$> e')
            ]
        )
      where
        (e', v) = eval e s
