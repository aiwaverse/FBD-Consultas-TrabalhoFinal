{-# LANGUAGE DerivingStrategies #-}

module Queries (Queries (..), netflixQueries) where

import Relude

newtype Queries = Queries (Map Int (String, Int)) deriving stock (Show)

netflixQueries :: Queries
netflixQueries =
  Queries $
    fromList
      [ (0, createQueryPair consultaSerie),
        (1, createQueryPair consultaEpisodio),
        (2, createQueryPair consultaContaConexao),
        (3, createQueryPair consultaParticipacao),
        (4, createQueryPair consultaItemMaisDeUmaCategoria),
        (5, createQueryPair consultaPessoasNuncaAtores),
        (6, createQueryPair consultaPessoasFilmesESeries),
        (7, createQueryPair consultaItemsTodasCategorias),
        (8, createQueryPair consultaMinhaLista),
        (9, createQueryPair consultaItemCategoria)
      ]

-- | Counts the occurence of an item on a foldable
-- Intended for lists, but why not make it generic
-- >>> count [1..10] 2
-- 1
-- >>> count "banana" 'a'
-- 3
count :: (Foldable t, Eq a) => t a -> a -> Int
count f e = foldr (\ls a -> if ls == e then a + 1 else a) 0 f

-- | Creates a pair with the query and the amount of occurences of '?'
-- >>> createQueryPair consultaSerie
-- ("select titulo, descricao, anolancamento, nome as classificacaofrom ItemCatalogojoin Serieusing (idCatalogo)join ClassificacaoIndicativausing (idClassificacao)where titulo like ?",1)
createQueryPair :: String -> (String, Int)
createQueryPair q = (q, count q '?')

consultaSerie :: String
consultaSerie =
  "select titulo \
  \, descricao \
  \, anolancamento \
  \, nome as classificacao \
  \from ItemCatalogo \
  \join Serie \
  \using (idCatalogo) \
  \join ClassificacaoIndicativa \
  \using (idClassificacao) \
  \where titulo like ?"

consultaEpisodio :: String
consultaEpisodio =
  "select Episodio.nome \
  \, temporada \
  \, numero as numeroepisodio \
  \, ItemCatalogo.titulo as tituloSerie \
  \from episodio \
  \join serie \
  \using (idCatalogo) \
  \join ItemCatalogo \
  \using (idCatalogo) \
  \where titulo like ? \
  \order by tituloSerie, temporada, numero"

consultaContaConexao :: String
consultaContaConexao =
  "select Cliente.nome \
  \, count(idConta) as contas \
  \, count(idDispositivo) as dispositivos \
  \from Cliente \
  \join Conta \
  \using (idCliente) \
  \full join conexao \
  \using (idCliente) \
  \where Cliente.nome like ? \
  \group by (idCliente, Cliente.nome)"

consultaParticipacao :: String
consultaParticipacao =
  "select pessoa \
  \, papel \
  \, titulo \
  \, descricao \
  \, nome as classificacaoIndicativa \
  \from v_PessoaPapel \
  \join ItemCatalogo \
  \using (idCatalogo) \
  \join ClassificacaoIndicativa \
  \using (idClassificacao) \
  \where pessoa like ?"

consultaItemMaisDeUmaCategoria :: String
consultaItemMaisDeUmaCategoria =
  "select titulo \
  \, count(idCategoria) as categorias \
  \from ItemCatalogo \
  \join Categorizacao \
  \using (idCatalogo) \
  \join Categoria \
  \using (idCategoria) \
  \where titulo like ? \
  \group by (idCatalogo, titulo) \
  \having count(idCategoria) > 1"

consultaPessoasNuncaAtores :: String
consultaPessoasNuncaAtores =
  "select titulo \
  \, ClassificacaoIndicativa.nome as Classificacao \
  \, pessoa \
  \, papel \
  \from ItemCatalogo \
  \join v_PessoaPapel \
  \using (idCatalogo) \
  \join ClassificacaoIndicativa \
  \using (idClassificacao) \
  \where pessoa like ? and idPessoa not in ( \
  \select idPessoa \
  \from v_PessoaPapel \
  \where papel = 'Ator' \
  \)"

consultaPessoasFilmesESeries :: String
consultaPessoasFilmesESeries =
  "select pessoa \
  \, papel \
  \from v_PessoaPapel \
  \join Serie \
  \using (idCatalogo) \
  \where idPessoa in ( \
  \select idPessoa \
  \from v_PessoaPapel \
  \join Visualizavel \
  \using (idCatalogo) \
  \join Filme \
  \using (idVisualizavel) \
  \)"

consultaItemsTodasCategorias :: String
consultaItemsTodasCategorias =
  "select titulo \
  \, descricao \
  \, nome as classificacao \
  \from ItemCatalogo i1 \
  \join ClassificacaoIndicativa \
  \using (idClassificacao) \
  \where titulo <> ? \
  \and not exists ( \
  \select * \
  \from Categorizacao \
  \join ItemCatalogo \
  \using (idCatalogo) \
  \where titulo = ? \
  \and idCategoria not in ( \
  \select \
  \distinct idCategoria \
  \from Categorizacao \
  \where idCatalogo = i1.idCatalogo \
  \) \
  \)"

consultaMinhaLista :: String
consultaMinhaLista =
  "select Conta.nome as conta \
  \, titulo \
  \from Conta \
  \join MinhaLista \
  \using (idConta) \
  \join ItemCatalogo \
  \using (idCatalogo) \
  \where titulo like ?"

consultaItemCategoria :: String
consultaItemCategoria =
  "select distinct \
  \Conta.nome as conta \
  \, titulo \
  \, Categoria.nome as categoria \
  \from Conta \
  \join Historico \
  \using (idConta) \
  \join Visualizavel \
  \using (idVisualizavel) \
  \join ItemCatalogo \
  \using (idCatalogo) \
  \join Categorizacao \
  \using (idCatalogo) \
  \join Categoria \
  \using (idCategoria) \
  \where Categoria.nome like ?"
