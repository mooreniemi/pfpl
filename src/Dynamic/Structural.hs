module Dynamic.Structural where
import Static.Checker

interpret :: EExp -> EExp
interpret ast = case ast of
                  (ENum digit) -> ENum digit
                  (EStr string) -> EStr string
                  (EAdd (ENum first) (ENum second)) -> ENum (first + second)
                  (EMult (ENum first) (ENum second)) -> ENum (first * second)
                  (ECat (EStr first) (EStr second)) -> EStr (first ++ second)
                  _ -> normalize ast
                  where
                    normalize :: EExp -> EExp
                    normalize expression = case expression of
                                             EAdd e1 e2 -> interpret (EAdd (interpret e1) (interpret e2))
                                             ECat e1 e2 -> interpret (ECat (interpret e1) (interpret e2))
                                             (EDef value identifier expBody) -> case expBody of
                                                   EId _ -> value
                                                   -- we chuck out the identifier here as we only have one
                                                   -- only one EId in an EDef
                                                   _ -> interpret (substitute value expBody)
                                                   where
                                                        substitute :: EExp -> EExp -> EExp
                                                        substitute x b = case b of
                                                          (EAdd (ENum e1) (EId _)) -> (EAdd (ENum e1) x)
                                                          (EAdd (EId _) (ENum e2)) -> (EAdd x (ENum e2))
                                                          (EMult (ENum e1) (EId _)) -> (EMult (ENum e1) x)
                                                          (EMult (EId _) (ENum e2)) -> (EMult x (ENum e2))
                                                          _ -> undefined
                                             _ -> undefined
