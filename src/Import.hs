{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET
   /busca BuscaR GET
   /listar ListaR GET
   /receita ReceitaR GET
   /cadastro CadastroR GET
   /cadastro/ingrediente CadIngreR GET POST
   /cadastro/receita CadReceitaR GET POST
   /cadastro/busca CadBuscaR GET POST
   /cadastro/categoria CadCateR GET POST
   /auto AutR GET POST
   /bye ByeR GET
|]