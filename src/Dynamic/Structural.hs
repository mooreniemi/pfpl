module Dynamic.Structural where
import Static.Checker

interpret :: EExp -> EExp
interpret ast = case ast of
                  (ENum digit) -> ENum digit
                  (EStr string) -> EStr string
                  (EAdd (ENum first) (ENum second)) -> ENum (first + second)
                  (ECat (EStr first) (EStr second)) -> EStr (first ++ second)
                  (EDef (EStr value) identifier (EId body)) -> EStr value
                  (EDef (ENum value) identifier (EId body)) -> ENum value
                  _ -> normalize ast
                  where
                    normalize :: EExp -> EExp
                    normalize expression = case expression of
                                             EAdd e1 e2 -> interpret (EAdd (interpret e1) (interpret e2))
                                             ECat e1 e2 -> interpret (ECat (interpret e1) (interpret e2))
                                             (EDef value identifier exp) -> interpret (EDef value identifier (interpret exp))
                                             _ -> undefined
