{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod

pRoutes = [parseRoutes|
   / HomeR GET
   /busca BuscaR GET
   /listar ListaR GET
   /receita/#ReceitaId ReceitaR GET
   /listar/ingrediente ListIngreR GET
   /listar/receita ListReceitaR GET
   /listar/categoria ListCateR GET
   /cadastro CadastroR GET
   /cadastro/ingrediente CadIngreR GET POST
   /cadastro/receita CadReceitaR GET POST
   /cadastro/busca CadBuscaR GET POST
   /cadastro/categoria CadCateR GET POST
   /auto AutR GET POST
   /creditos CreditoR GET
   /bye ByeR GET
|]