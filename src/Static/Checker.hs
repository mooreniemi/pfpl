{-# LANGUAGE QuasiQuotes #-}
module Static.Checker where
import qualified Data.Map.Strict as Map
import Data.String.Interpolate

data EType = TNum
           | TStr
           deriving (Show, Eq)

data EExp = EId String
          | ENum Int
          | EStr String
          | EAdd EExp EExp
          | EMult EExp EExp
          | ECon EExp EExp
          | ELen EExp
          | EDef { value :: EExp
                 , identifier :: String
                 , body :: EExp
                 }
          deriving (Show, Eq)

type TypeEnv = Map.Map String EType

check' :: EExp -> Either String EType
check' = check Map.empty

check :: TypeEnv -> EExp -> Either String EType
check typeEnv expression = case expression of
  EId stringId -> case Map.lookup stringId typeEnv of
    Just typeValue -> Right typeValue
    Nothing -> Left [i|No match in Type Environment found for: #{stringId}|]
  ENum _ -> Right TNum
  EStr _ -> Right TStr
  EAdd (ENum _) (ENum _) -> Right TNum
  EMult (ENum _) (ENum _) -> Right TNum
  ECon (EStr _) (EStr _) -> Right TStr
  ELen (EStr _) -> Right TNum
  ELen expr@(EId stringId) -> let typeOfExpr = check typeEnv expr
                              in case typeOfExpr of
                                  Right TStr -> Right TNum
                                  e -> Left [i|Can only take length of strings, #{e} was not EStr|]
  EDef expr1 stringId expr2 ->
    case check typeEnv expr1 of
      Right typeOfExpr1 -> let typeEnv' = Map.insert stringId typeOfExpr1 typeEnv
                           in check typeEnv' expr2
      Left e -> Left e
  EAdd expr1 expr2 -> Left [i|EAdd requires two ENums, but received #{expr1} and #{expr2}|]
  EMult expr1 expr2 -> Left [i|EMult requires two ENums, but received #{expr1} and #{expr2}|]
  ECon expr1 expr2 -> Left [i|ECon requires two EStrs, but received #{expr1} and #{expr2}|]
  e -> Left [i|Failed to match any patterns with: #{e}|] 
