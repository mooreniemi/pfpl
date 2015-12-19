module Static.Checker where

data EType = TNum
           | TStr
           deriving (Show, Eq)

data EExp = ENum Int
          | EStr String
          | EAdd EExp EExp
          | EMult EExp EExp
          | ECon EExp EExp
          | ELen EExp
          | EDef EExp String EExp
          deriving (Show, Eq)

check :: EExp -> EType
check expression = case expression of
  ENum _ -> TNum
  _ -> undefined

