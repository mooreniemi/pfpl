module Dynamic.Structural where
import Static.Checker

interpret :: EExp -> EExp
interpret (ENum digit) = ENum digit
interpret (EStr string) = EStr string
interpret (EAdd (ENum first) (ENum second)) = ENum (first + second) 
