module Menu (getInput, getProtectedInput, mainMenu, initial) where

import Relude
import System.IO (hSetEcho)
import System.Process

getInput :: IO Text
getInput = getLine

getProtectedInput :: IO Text
getProtectedInput = do
  hSetEcho stdin False
  r <- getLine
  hSetEcho stdin True
  pure r

mainMenu :: IO ()
mainMenu = do
  _ <- system "clear"
  putTextLn "Bem vindo as consultas da base da Netflix!"
  putTextLn "Por favor escolha sua opção:"
  putTextLn "0 - Consulta por nome da Série"
  putTextLn "1 - Consulta de Episódios por nome da Série"
  putTextLn "2 - Consulta de Dispositivos e Contas por nome do Cliente"
  putTextLn "3 - Consulta de Pessoas e Participações"
  putTextLn "4 - Consulta de Items de Catalogo que possuam mais de uma categoria"
  putTextLn "5 - Consulta de Pessoas que nunca foram atores"
  putTextLn "6 - Consulta de Pessoas que participaram de ao menos um filme e uma série"
  putTextLn "7 - Consulta de Itens que possuam todas as categorias de outro item"
  putTextLn "8 - Consulta de 'Minha Lista'"
  putTextLn "9 - Consulta de Itens por categoria"

initial :: IO (Text, Text, Text)
initial = do
  putTextLn "Por favor entre com o nome da base de dados."
  db <- getInput
  putTextLn "Por favor entre com o usuário da base de dados."
  user <- getInput
  putTextLn "Por favor entre com a senha do usuário da base de dados."
  password <- getProtectedInput
  pure (db, user, password)