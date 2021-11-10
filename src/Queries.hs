module Queries where
import Relude


consultaSerie :: String
consultaSerie = "select titulo \
                \     , descricao \
                \     , anolancamento \
                \     , nome as classificacao \
                \from ItemCatalogo \
                \join Serie \
                \using (idCatalogo) \
                \join ClassificacaoIndicativa \
                \using (idClassificacao) \
                \where titulo like ?"

consultaEpisodio :: String
consultaEpisodio = "select Episodio.nome \
                        \, temporada\
                        \, numero as numeroepisodio\
                        \, ItemCatalogo.titulo as tituloSerie\
                        \from episodio\
                        \join serie\
                        \using (idCatalogo)\
                        \join ItemCatalogo\
                        \using (idCatalogo)\
                        \where titulo like ?\
                        \order by tituloSerie, temporada, numero"