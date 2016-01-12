module Dynamic.Structural where
import Static.Checker

interpret :: EExp -> EExp
interpret ast = case ast of
                  (ENum digit) -> ENum digit
                  (EStr string) -> EStr string
                  (EAdd (ENum first) (ENum second)) -> ENum (first + second)
                  (ECat (EStr first) (EStr second)) -> EStr (first ++ second)
                  (ECat (e1) (EStr second)) -> interpret ((ECat (interpret e1) (EStr second)))
                  (ECat (EStr first) (e2)) -> interpret ((ECat (EStr first) (interpret e2)))
                  _ -> normalize ast 
                  where
                    normalize :: EExp -> EExp
                    normalize expression = case expression of
                                             EAdd e1 e2 -> interpret (EAdd (interpret e1) (interpret e2))
                                             _ -> undefined
