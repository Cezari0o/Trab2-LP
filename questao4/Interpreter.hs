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
                     Right numResult -> Right (update context (getStr id) numResult)
   ------------------------------------------------------------------
   SBlock [] -> Right context
   SBlock (s:stms) -> case (execute context s) of
                        Left msg -> Left msg
                        Right result -> execute result (SBlock stms)
   ------------------------------------------------------------------
   SWhile exp stm -> case (eval context exp) of
                        Left errorMsg -> Left errorMsg
                        Right numResult -> if (numResult /= 0)
                                          then (case (execute context stm) of
                                                   Left stmErrorMsg -> Left stmErrorMsg
                                                   Right stmContextResult -> execute stmContextResult (SWhile exp stm))
                                          else Right context
   ------------------------------------------------------------------
   STry (sTry:stmsTry) stmtsCatch stmtsFinal -> case (execute context sTry) of
                                                         Left _ -> case (execute context (SBlock stmtsCatch)) of
                                                                     Left errorMsg -> Left errorMsg
                                                                     Right catchContextResult -> execute catchContextResult (SBlock stmtsFinal)
                                                         Right tryContextResult -> execute tryContextResult (STry stmsTry stmtsCatch stmtsFinal)
   STry [] stmtsCatch stmtsFinal -> execute context (SBlock stmtsFinal)

-- Funcao usada por eval. Dado um contexto, duas expressoes e uma funcao,
-- retorna (Either ErrorMessage Integer)
getResult context exp0 exp funcao = case eval context exp0 of
                        Left msg0 -> Left msg0
                        Right result0 -> (case eval context exp of
                                             Left msg1 -> Left msg1
                                             Right result1 -> Right (funcao result0 result1))

-- Dado um contexto e uma expressao, retorna um inteiro ou uma mensagem de erro,
-- resultante da execucao da expressao.
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
    EAdd exp0 exp  -> getResult context exp0 exp (+)
    ESub exp0 exp  -> getResult context exp0 exp (-)
    EMul exp0 exp  -> getResult context exp0 exp (*) 
    EDiv exp0 exp -> (case (eval context exp0) of
                        Right n -> (case (eval context exp) of 
                                       Left msg1 -> Left msg1
                                       Right m -> if (m /= 0)
                                                   then Right (n `div` m) 
                                                   else Left ("divisao por 0"))
                        Left msg -> Left msg)
    EInt n  -> Right n
    EVar id  -> Right (lookup context (getStr id))

-- Dica: voce nao precisa mudar o codigo a partir daqui
type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

-- Procura o identificador dado pela string no contexto recebido.
-- Retorna o valor associado ao identificador.
lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

-- Atualiza o contexto, associando o identificador ao valor recebido
-- caso o mesmo nao se encontre no contexto, ou atualizando o valor associado
-- ao identificador.
update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
