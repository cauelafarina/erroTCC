{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static() -- imagens

pRoutes = [parseRoutes|
   / HomeR GET
   /busca BuscaR GET
   /listar ListaR GET
   /receita/#ReceitaId ReceitaR GET
   /categoria/#CategoriaId CategoriaR GET
   /listar/ingrediente ListIngreR GET
   /listar/receita ListReceitaR GET
   /listar/categoria ListCateR GET
   /cadastro CadastroR GET
   /cadastro/ingrediente CadIngreR GET POST
   /cadastro/receita CadReceitaR GET POST
   /cadastro/busca CadBuscaR GET POST
   /cadastro/categoria CadCateR GET POST
   /cadastro/usuario CadUseR GET POST
   /login LoginR GET POST
   /creditos CreditoR GET
   /bye ByeR GET
   /static StaticR Static getStatic
|]