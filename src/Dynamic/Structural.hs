module Dynamic.Structural where
import Static.Checker

interpret :: EExp -> EExp
interpret (ENum digit) = ENum digit
