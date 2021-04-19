module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar conforme a sugestao abaixo, 
   mas a sua definicao (corpo) pode ficar a mesma
   executeP :: RContext -> Program  -> Either ErrorMessage RContext
-}
-- executeP :: RContext -> Program  -> RContext
executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   

{- Dica: o tipo de execute deve mudar para 
 execute :: RContext -> Stm -> Either ErrorMessage RContext   
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos 
 serao afetados
-}


-- execute :: RContext -> Stm -> RContext
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
   SAss id exp -> case (eval context exp) of
                     Left msg -> Left msg
                     Right result -> Right (update context (getStr id) result)
   SBlock [] -> Right context
   SBlock (s:stms) -> case (execute context s) of
                        Left msg -> Left msg
                        Right result -> execute result (SBlock stms)
-- execute (execute context s) (SBlock stms)

   SWhile exp stm -> case (eval context exp) of
                        Left msg -> Left msg
                        Right result -> if (result /= 0)
                                          then (case (execute context stm) of
                                                   Left stmMsg -> Left stmMsg
                                                   Right stmResult -> execute stmResult (SWhile exp stm))
                                          else Right context
      -- if ( (eval context exp) /= 0) 
      --                 then execute (execute context stm) (SWhile exp stm)
      --                 else Right context


getResult context exp0 exp funcao = case eval context exp0 of
                        Left msg0 -> Left msg0
                        Right result0 -> (case eval context exp of
                                             Left msg1 -> Left msg1
                                             Right result1 -> Right (funcao result0 result1))


{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
    EAdd exp0 exp  -> getResult context exp0 exp (+)
      --  (eval context exp0 + eval context exp)
    ESub exp0 exp  -> getResult context exp0 exp (-)
      --  (eval context exp0 - eval context exp)
    EMul exp0 exp  -> getResult context exp0 exp (*) 
      --  (eval context exp0 * eval context exp)
   --  EDiv exp0 exp  -> eval context exp0 `div` eval context exp
   -- (5 + 7 / 0) / (7 + 1 * 4)
    EDiv exp0 exp -> (case (eval context exp0) of
                        Right n -> (case (eval context exp) of 
                                       Left msg1 -> Left msg1
                                       Right m -> if (m /= 0)
                                                   then Right (n `div` m) 
                                                   else Left ("divisao por 0"))
                        Left msg -> Left msg)
                        -- EInt 0 -> Left ("divisao por 0, cabaco: " ++ (show (EDiv exp0 exp)))
                        -- _ -> Right (eval context exp0 `div` eval context exp)
    EInt n  -> Right n
    EVar id  -> Right (lookup context (getStr id))
{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of 
                    Right ve1 -> case eval context e2 of 
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: " 
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg  
                    Left msg -> Left msg  
    EInt n  ->  Right n 
-}                


-- Dica: voce nao precisa mudar o codigo a partir daqui
type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv