{-# LANGUAGE QuasiQuotes #-}
-- pragma necessary for string interpolation via Interpolate

module Static.Checker where
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Control.Monad

data EType = TNum
           | TStr
           deriving (Show, Eq)

-- introductory forms for a type determine the values, or canonical forms, of that type
-- ie ENum & EStr
-- eliminatory forms for a type determine how to manipulate the values
-- of a type to form a computation of another (possibly the same) type
-- ie EMult & EAdd for nums, and ECat and ELen for strings
data EExp = EId String
          | ENum Int
          | EStr String
          | EAdd EExp EExp
          | EMult EExp EExp
          | ECat EExp EExp
          | ELen EExp
          | EDef { value :: EExp
                 , identifier :: String
                 , body :: EExp
                 }
          deriving (Show, Eq)

-- corresponds to Gamma or "typing context"
type TypeEnv = Map.Map String EType
-- Map ensures Lemma 4.1 (Unicity of typing)

check' :: EExp -> Either String EType
check' = check Map.empty

check :: TypeEnv -> EExp -> Either String EType
check typeEnv expression = case expression of
  -- 4.1a
  EId stringId -> case Map.lookup stringId typeEnv of
    Just typeValue -> Right typeValue
    Nothing -> Left [i|No match in Type Environment found for: #{stringId}|]
  -- 4.1b
  EStr _ -> Right TStr
  -- 4.1c
  ENum _ -> Right TNum
  -- 4.1d
  EAdd expr1 expr2 -> do
     _ <- confirmType TNum expr1
     confirmType TNum expr2
  -- 4.1e
  EMult expr1 expr2 -> do
    _ <- confirmType TNum expr1
    confirmType TNum expr2
  -- 4.1f
  ECat expr1 expr2 -> do
    _ <- confirmType TStr expr1
    confirmType TStr expr2
  -- 4.1g
  ELen (EStr _) -> Right TNum
  ELen expr@(EId stringId) -> let typeOfExpr = check typeEnv expr
                              in case typeOfExpr of
                                  Right TStr -> Right TNum
                                  e -> Left [i|Can only take length of strings, #{e} was not EStr|]
  -- 4.1h
  EDef expr1 stringId expr2 ->
    case check typeEnv expr1 of
      Right typeOfExpr1 -> let typeEnv' = Map.insert stringId typeOfExpr1 typeEnv
                           in check typeEnv' expr2
      Left e -> Left e
  e -> Left [i|Failed to match any patterns with: #{e}|]
  where
    confirmType :: EType -> EExp -> Either String EType
    confirmType desiredType expr = do
      typeOfExpr <- check typeEnv expr
      when (typeOfExpr /= desiredType) $
        Left [i|#{expression} expected #{expr} to be #{desiredType}, but was #{typeOfExpr}.|]
      return typeOfExpr

