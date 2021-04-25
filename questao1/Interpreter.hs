module Interpreter where

import AbsLI
import Prelude hiding (lookup)


-- Dado um contexto e um programa, executa o programa.
-- Retorna o contexto resultante.
executeP :: RContext -> Program  -> RContext
executeP context (Prog stm) = execute context stm
   
-- Dado um contexto e um statement, executa o dado statement no contexto.
-- Retorna o contexto resultante da execucao do statement.
execute :: RContext -> Stm -> RContext
execute context x = case x of
   SAss id exp -> update context (getStr id) (eval context exp)
   SBlock [] -> context
   SBlock (s:stms) -> execute (execute context s) (SBlock stms) 
   SWhile exp stm -> if ( (eval context exp) /= 0) 
                      then execute (execute context stm) (SWhile exp stm)
                      else context
   ---------------------------------------------------------------------------
   -- O do/while abaixo executa o statement uma vez, e depois eh tratado como um while
   SdoWhile stm exp -> execute (execute context stm) (SWhile exp stm)   


-- Dado um contexto e uma expressao, retorna um inteiro,
-- resultante da execucao da expressao.
eval :: RContext -> Exp -> Integer
eval context x = case x of
    EAdd exp0 exp  -> eval context exp0 + eval context exp
    ESub exp0 exp  -> eval context exp0 - eval context exp
    EMul exp0 exp  -> eval context exp0 * eval context exp
    EDiv exp0 exp  -> eval context exp0 `div` eval context exp
    EInt n  -> n
    EVar id  -> lookup context (getStr id)

type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

-- Procura o identificador dado pela string no contexto recebido.
-- Retorna o inteiro associado ao identificador.
lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

-- Atualiza o contexto, associando o identificador ao inteiro recebido
-- caso o mesmo nao se encontre no contexto, ou atualizando o inteiro associado
-- ao identificador.
update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
