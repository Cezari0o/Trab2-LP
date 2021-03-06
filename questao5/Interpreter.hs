module Interpreter where

import AbsLI
import Prelude hiding (lookup)

type ErrorMessage = String

-- Dado um contexto e um programa, executa o programa.
-- Retorna o contexto resultante ou uma mensagem de erro de execucao.
executeP :: RContext -> Program  -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   

-- Dado um contexto e um statement, executa o dado statement no contexto.
-- Retorna o contexto resultante da execucao do statement ou uma mensagem de erro de execucao.
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
   SAss id exp -> case (eval context exp) of
                     Left errorMsg -> Left errorMsg
                     Right valorResult -> Right (update context (getStr id) valorResult)
   ------------------------------------------------------------------
   SBlock [] -> Right context
   SBlock (s:stms) -> case (execute context s) of
                        Left msg -> Left msg
                        Right result -> execute result (SBlock stms)
   ------------------------------------------------------------------
   SWhile exp stm -> case (eval context exp) of
                        Left errorMsg -> Left errorMsg
                        Right valorResult -> if (i(valorResult) /= 0)
                                          then (case (execute context stm) of
                                                   Left stmErrorMsg -> Left stmErrorMsg
                                                   Right stmContextResult -> execute stmContextResult (SWhile exp stm))
                                          else Right context
   ------------------------------------------------------------------ 
   SdoWhile stm exp -> case (execute context stm) of 
                        Left errorMsgDo -> Left errorMsgDo
                        Right stmContextResultDo -> execute stmContextResultDo (SWhile exp stm) 
   ------------------------------------------------------------------
   STry (sTry:stmsTry) stmtsCatch stmtsFinal -> case (execute context sTry) of
                                                         Left _ -> case (execute context (SBlock stmtsCatch)) of
                                                                     Left errorMsg -> Left errorMsg
                                                                     Right catchContextResult -> execute catchContextResult (SBlock stmtsFinal)
                                                         Right tryContextResult -> execute tryContextResult (STry stmsTry stmtsCatch stmtsFinal)
   STry [] stmtsCatch stmtsFinal -> execute context (SBlock stmtsFinal)


s :: Valor -> String             
s (ValorStr str) = str
i :: Valor -> Integer
i (ValorInt vint) = vint 
b :: Valor -> Bool
b (ValorBool vbool) = vbool

getValorInt :: Integer -> Valor
getValorInt n = ValorInt n

getValorBool :: Bool -> Valor
getValorBool b = ValorBool b

getValorStr :: String -> Valor
getValorStr s = ValorStr s

-- Funcao usada por eval. Dado um contexto, duas expressoes, uma operacao, um extrator do tipo (Valor -> t)
-- e uma funcao getValor do tipo (t -> Valor), retorna (Either ErrorMessage Valor)
getResult context exp0 exp operacao extratorT getValor = 
    case eval context exp0 of
        Left msg0 -> Left msg0
        Right result0 -> (case eval context exp of
                                Left msg1 -> Left msg1
                                Right result1 -> Right (getValor((extratorT result0) `operacao` (extratorT result1))))

-- Dado um contexto e uma expressao, retorna um valor ou uma mensagem de erro,
-- resultante da execucao da expressao.
eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of 
    EAdd exp0 exp  -> getResult context exp0 exp (+) i getValorInt
    ------------------------------------------------------------------
    ESub exp0 exp  -> getResult context exp0 exp (-) i getValorInt
    ------------------------------------------------------------------
    EMul exp0 exp  -> getResult context exp0 exp (*) i getValorInt
    ------------------------------------------------------------------
    EDiv exp0 exp -> (case (eval context exp0) of
                        Right (ValorInt n) -> (case (eval context exp) of 
                                       Left msg1 -> Left msg1
                                       Right (ValorInt m) -> if (m /= 0)
                                                   then Right (ValorInt (n `div` m)) 
                                                   else Left ("divisao por 0"))
                        Left msg -> Left msg)
    ------------------------------------------------------------------
    EInt n   -> Right (ValorInt n)
    ------------------------------------------------------------------
    EVar id  -> Right (lookup context (getStr id))
    ------------------------------------------------------------------
    EOr exp0 exp  -> getResult context exp0 exp (||) b getValorBool
    ------------------------------------------------------------------
    EAnd exp0 exp -> getResult context exp0 exp (&&) b getValorBool
    ------------------------------------------------------------------
    ECon exp0 exp -> getResult context exp0 exp (++) s getValorStr   
    ------------------------------------------------------------------
    ENot exp -> case eval context exp of
                    Left errorMsg -> Left errorMsg
                    Right valor   ->  Right (ValorBool (not (b valor)))
    ------------------------------------------------------------------
    EStr exp -> Right (ValorStr exp) 
    ------------------------------------------------------------------
    ETrue    -> Right (ValorBool True)
    ------------------------------------------------------------------
    EFalse   -> Right (ValorBool False) 

data Valor = ValorStr String |
             ValorBool Bool  |
             ValorInt Integer

instance Show Valor where
 show (ValorInt vint) = show vint
 show (ValorStr vstr) = vstr
 show (ValorBool vb) = show vb

-- precisamos que Valor esteja em Eq para podermos especificar os casos de teste em Testes.hs
instance Eq Valor where
 (ValorInt i1) == (ValorInt i2) =  i1 == i2
 (ValorStr s1) == (ValorStr s2) =  s1 == s2
 (ValorBool b1) == (ValorBool b2) = b1 == b2

type RContext = [(String,Valor)]

getStr :: Ident -> String
getStr (Ident s) = s

-- Procura o identificador dado pela string no contexto recebido.
-- Retorna o valor associado ao identificador.
lookup :: RContext -> String -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

-- Atualiza o contexto, associando o identificador ao valor recebido
-- caso o mesmo nao se encontre no contexto, ou atualizando o valor associado
-- ao identificador.
update :: RContext -> String -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv