module Static.Checker where
import qualified Data.Map.Strict as Map

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

check' :: EExp -> EType
check' = check Map.empty

check :: TypeEnv -> EExp -> EType
check typeEnv expression = case expression of
  EId stringId -> case Map.lookup stringId typeEnv of
    Just typeValue -> typeValue
    Nothing -> undefined
  ENum _ -> TNum
  EStr _ -> TStr
  EAdd (ENum _) (ENum _) -> TNum
  EMult (ENum _) (ENum _) -> TNum
  ECon (EStr _) (EStr _) -> TStr
  ELen (EStr _) -> TNum
  ELen expr@(EId stringId) -> let typeOfExpr = check typeEnv expr
                              in case typeOfExpr of
                                  TStr -> TNum
                                  e -> error' "Can only take length of strings." e
  EDef expr1 stringId expr2 -> let typeOfExpr1 = check typeEnv expr1
                                   typeEnv' = Map.insert stringId typeOfExpr1 typeEnv
                               in check typeEnv' expr2
  e -> error' "Failed to match:" e

error' :: Show a => String -> a -> b
error' message errorValue = error $ message ++ show errorValue
