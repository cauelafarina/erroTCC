{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where
import Import
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT) --ver criação do banco
import Control.Applicative --criar formulario
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes --dizer que o arquivo é só de handler

formCadIngre :: Form Ingrediente
formCadIngre = renderDivs $ Ingrediente <$>
               areq textField "Nome: " Nothing

formCadReceita :: Form Receita
formCadReceita = renderDivs $ Receita <$>
                 areq (selectField catg) "Categoria" Nothing <*>
                 areq textField "Nome: " Nothing <*>
                 areq textareaField "Descrição: " Nothing

catg = do
       entidades <- runDB $ selectList [] [Asc CategoriaNome]
       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades

formCadCateg :: Form Categoria
formCadCateg = renderDivs $ Categoria <$>
               areq textField "Nome: " Nothing

formCadBusca :: Form Busca
formCadBusca = renderDivs $ Busca <$>
               areq (selectField rec) "Receita: " Nothing <*>
               areq (selectField ing) "Ingredientes: " Nothing

rec = do
       entidades <- runDB $ selectList [] [Asc ReceitaNome] 
       optionsPairs $ fmap (\ent -> (receitaNome $ entityVal ent, entityKey ent)) entidades

ing = do
       entidades <- runDB $ selectList [] [Asc IngredienteNome] 
       optionsPairs $ fmap (\ent -> (ingredienteNome $ entityVal ent, entityKey ent)) entidades

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
              areq textField "Login" Nothing <*>
              areq textField "Senha" Nothing

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
     <div class="row">
           <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                      <h1>
                          <span class="glyphicon glyphicon-download-alt">
                          #{y}
     <div class="row">
            <div class="col-sm-6">
                <form method=post action=@{x} enctype=#{enctype}>
                    ^{widget}
                <input type="submit" value="Enviar">
|]

wHead :: String -> Widget
wHead title = toWidgetHead [hamlet|

    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <script src="https://code.jquery.com/jquery-2.1.4.min.js" crossorigin="anonymous">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" integrity="sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ==" crossorigin="anonymous">    
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" integrity="sha512-K1qjQ+NcF2TYO/eI3M6v8EiNYZfA95pQumfvcVrTHtwQVDG+aHRqLi/ETn2uB+1JqwYqVG3LIvdm9lj6imS/pQ==" crossorigin="anonymous">
    <title>#{title}
|]

wNavigation :: Widget
wNavigation = [whamlet|
    <nav class="navbar navbar-inverse">
        <div class="container-fluid">
            <div class="navbar-header">
                <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
                    <span class="sr-only">Toggle navigation
                    <span class="icon-bar">
                    <span class="icon-bar">
                    <span class="icon-bar">
                <a class="navbar-brand" href=@{HomeR}>
                   <span class="glyphicon glyphicon-home">
                   Início
            <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                <ul class="nav navbar-nav">
                    <li class="dropdown">
                      <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                        <span class="glyphicon glyphicon-list">
                        Listar
                        <span class="caret">
                      <ul class="dropdown-menu">
                        <li>
                          <a href=@{ListCateR}>Categorias
                        <li>
                          <a href=@{ListIngreR}>Ingredientes
                        <li>
                          <a href=@{ListReceitaR}>Receitas

                    <li class="dropdown">
                      <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                        <span class="glyphicon glyphicon-remove-circle">
                        Cadastro
                        <span class="caret">
                      <ul class="dropdown-menu">
                        <li>
                          <a href=@{CadIngreR}>Ingredientes
                        <li>
                          <a href=@{CadCateR}>Categorias
                        <li>
                          <a href=@{CadReceitaR}>Receita
                        <li>
                          <a href=@{CadBuscaR}>Busca

                <ul class="nav navbar-nav navbar-right">
                    <li>
                        <a href=@{CreditoR}>
                           <span class="glyphicon glyphicon-user">
                           Créditos
                    <li>
                        <a href=@{ByeR}>
                           <span class="glyphicon glyphicon-off">
                           Sair
|]

wContainer :: String -> Widget -> Widget
wContainer title content = do
    wHead title
    [whamlet|
    <div class="container">
         ^{wNavigation}
         ^{content}
|]

widgetWelcome :: Widget
widgetWelcome = [whamlet|

    <div class="row">
        <div class="col-sm-12">
            <div class="jumbotron">
               <h1>Do Armário à Geladeira
               <h2>Aqui você não passa fome!

|]


wCredito :: Widget
wCredito = do
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-user">
                        Desenvolvedores
        <div class="row">
            <div class="col-sm-6">
                <div class="table-responsive">
                    <table class="table">
                      <thead>
                        <tr>
                          <th>Alunos
                          <th>Matrícula
                      <tbody>
                        <tr>
                          <td>Cauê La Farina
                          <td>1310007-7
                        <tr>
                          <td>Guilherme Egídio
                          <td>........
                        <tr>
                          <td>Jorge Correa
                          <td>........
                        <tr>
                          <td>Julliana Alavarez
                          <td>........
    |]

wListReceitas :: Widget
wListReceitas = do
    listaR <- handlerToWidget $ runDB $ selectList [] [Asc ReceitaNome]
    [whamlet|
    <div class="row">
      <div class="col-sm-12">
        <div class="col-sm-12 page-header">
          <h1>
            <span class="glyphicon glyphicon-list-alt">
            Lista de Receitas
    <div class="row">
      <div class="col-sm-12">
          <div class="table-responsive">
            <table class="table">
              <thead>
                <tr>
                  <th>Receitas Cadastrados
              <tbody>
                $forall Entity rid receita <- listaR
                  <tr>
                    <td>
                        <a href=@{ReceitaR rid}>#{receitaNome receita}
    |]

wReceita :: ReceitaId -> Widget
wReceita rid = do
    receita <- handlerToWidget $ runDB $ get404 rid
    [whamlet|
        <div class="row">
            <div class="col-sm-12">
                <div class="col-sm-12 page-header">
                    <h1>
                        <span class="glyphicon glyphicon-registration-mark">
                        #{receitaNome receita}
        <div class="row">
            <div class="col-sm-6">
                <div class="table-responsive">
                    <table class="table">
                        <tbody>
                            <tr>
                                <th>Como fazer:
                                <td>#{receitaDescricao receita}

    |]

wListIngre :: Widget
wListIngre = do
    listaI <- handlerToWidget $ runDB $ selectList [] [Asc IngredienteNome]
    [whamlet|
    <div class="row">
      <div class="col-sm-12">
        <div class="col-sm-12 page-header">
          <h1>
            <span class="glyphicon glyphicon-list-alt">
            Lista de Ingredientes
    <div class="row">
      <div class="col-sm-12">
          <div class="table-responsive">
            <table class="table">
              <thead>
                <tr>
                  <th>Ingredientes Cadastrados
              <tbody>
                $forall Entity iid ingrediente <- listaI
                  <tr>
                    <td>
                        <p>#{ingredienteNome ingrediente}
    |]

wListCate :: Widget
wListCate = do
    listaC <- handlerToWidget $ runDB $ selectList [] [Asc CategoriaNome]
    [whamlet|
    <div class="row">
      <div class="col-sm-12">
        <div class="col-sm-12 page-header">
          <h1>
            <span class="glyphicon glyphicon-list-alt">
            Lista de Categorias
    <div class="row">
      <div class="col-sm-12">
          <div class="table-responsive">
            <table class="table">
              <thead>
                <tr>
                  <th>Categorias Cadastradas
              <tbody>
                $forall Entity cid categoria <- listaC
                  <tr>
                    <td>
                        <p>#{categoriaNome categoria}
    |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do (wContainer "Home" widgetWelcome)

getBuscaR :: Handler Html
getBuscaR = undefined

getListaR :: Handler Html
getListaR = undefined

getCadastroR :: Handler Html
getCadastroR = undefined

getCreditoR :: Handler Html
getCreditoR = defaultLayout $ do (wContainer "Créditos" wCredito)

-- GET POST cadastro de Ingredientes
getCadIngreR :: Handler Html
getCadIngreR = do
             (widget, enctype) <- generateFormPost formCadIngre
             defaultLayout $ do (wContainer "Cadastro de Ingredientes" (widgetForm CadIngreR enctype widget "Cadastro de Ingredientes"))


postCadIngreR :: Handler Html
postCadIngreR = do
                ((result, _), _) <- runFormPost formCadIngre
                case result of
                    FormSuccess ingre -> do
                       runDB $ insert ingre
                       defaultLayout [whamlet|
                           <h1> #{ingredienteNome ingre} inserido com sucesso. <br>
                           <a href=@{CadIngreR}> Voltar
                       |]
                    _ -> redirect CadIngreR
-- GET POST cadastro de Ingredientes

-- GET POST cadastro de Receitas
getCadReceitaR :: Handler Html
getCadReceitaR = do
             (widget, enctype) <- generateFormPost formCadReceita
             defaultLayout $ do (wContainer "Cadastro de Receitas" (widgetForm CadReceitaR enctype widget "Cadastro de Receitas"))

postCadReceitaR :: Handler Html
postCadReceitaR = do
                ((result, _), _) <- runFormPost formCadReceita
                case result of
                    FormSuccess receita -> do
                       runDB $ insert receita
                       defaultLayout [whamlet|
                           <h1> #{receitaNome receita} inserido com sucesso. <br>
                           <a href=@{CadReceitaR}> Voltar
                       |]
                    _ -> redirect CadReceitaR
-- GET POST cadastro de Receitas

-- GET POST cadastro de Busca
getCadBuscaR :: Handler Html
getCadBuscaR = do
             (widget, enctype) <- generateFormPost formCadBusca
             defaultLayout $ do (wContainer "Cadastro de Receitas" (widgetForm CadBuscaR enctype widget "Cadastro de Buscas"))

postCadBuscaR :: Handler Html
postCadBuscaR = do
                ((result, _), _) <- runFormPost formCadBusca
                case result of
                    FormSuccess busca -> do
                       runDB $ insert busca
                       defaultLayout [whamlet|
                           <h1> Busca inserida com sucesso. <br>
                           <a href=@{CadBuscaR}> Voltar
                       |]
                    _ -> redirect CadBuscaR
-- GET POST cadastro de Busca


-- GET POST cadastro de Categorias
getCadCateR :: Handler Html
getCadCateR = do
             (widget, enctype) <- generateFormPost formCadCateg
             defaultLayout $ do (wContainer "Cadastro de Receitas" (widgetForm CadCateR enctype widget "Cadastro de Categorias"))

postCadCateR :: Handler Html
postCadCateR = do
                ((result, _), _) <- runFormPost formCadCateg
                case result of
                    FormSuccess categoria -> do
                       runDB $ insert categoria
                       defaultLayout [whamlet|
                           <h1> #{categoriaNome categoria} inserida com sucesso. <br>
                           <a href=@{CadCateR}> Voltar
                       |]
                    _ -> redirect CadCateR
-- GET POST cadastro de Categorias


getListIngreR :: Handler Html
getListIngreR = defaultLayout $ do (wContainer "Lista de Ingredientes" wListIngre)

getListReceitaR :: Handler Html
getListReceitaR = defaultLayout $ do (wContainer "Lista de Receitas" wListReceitas)

getReceitaR :: ReceitaId -> Handler Html
getReceitaR rid = defaultLayout $ do (wContainer "Info Receita" (wReceita rid))

getListCateR :: Handler Html
getListCateR = defaultLayout $ do (wContainer "Lista de Categorias" wListCate)

getAutR :: Handler Html
getAutR = do
          (widget, enctype) <- generateFormPost formUsuario
          defaultLayout $ do (wContainer "Login" (widgetForm AutR enctype widget "Login"))

postAutR :: Handler Html
postAutR = do
           ((result, _), _) <- runFormPost formUsuario
           case result of
                    FormSuccess user -> do
                       setSession "_ID" (usuarioNome user)
                       redirect CadIngreR
                    _ -> redirect AutR

getByeR :: Handler Html
getByeR = do
          deleteSession "_ID"
          defaultLayout $ (wContainer "Login" ([whamlet| BYE! <br>
                        <a href=@{HomeR}> Voltar|]))

connStr = "dbname=ddniie89e3rtk3 host=ec2-54-225-199-108.compute-1.amazonaws.com user=haldvwbpgvjigm password=PQOXmc2BAD8zxzmBJmDx7KPMh6 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 $ Sitio pool
--       warpEnv (Sitio pool)