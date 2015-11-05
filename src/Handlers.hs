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
            <h1>
                #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enviar">
|] >> toWidget [lucius|
       label{
          color:blue;
       }
|]

widgetImports :: Widget
widgetImports = [whamlet|

    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" integrity="sha512-dTfge/zgoMYpP7QbHy4gWMEGsbsdZeCXz7irItjcC3sPUFtf0kuFbDz/ixG7ArTxmDjLXDmezHubeNikyKGVyQ==" crossorigin="anonymous">
    <script src="https://code.jquery.com/jquery-2.1.4.min.js" crossorigin="anonymous">
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" integrity="sha512-K1qjQ+NcF2TYO/eI3M6v8EiNYZfA95pQumfvcVrTHtwQVDG+aHRqLi/ETn2uB+1JqwYqVG3LIvdm9lj6imS/pQ==" crossorigin="anonymous">

    <nav class="navbar navbar-default">
        <div class="container">
            <div class="navbar-header">
                <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
                    <span class="sr-only">Toggle navigation
                    <span class="icon-bar">
                    <span class="icon-bar">
                    <span class="icon-bar">
                <a class="navbar-brand" href=@{HomeR}>Home
            <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                <ul class="nav navbar-nav">
                    <li>
                        <a href=@{BuscaR}>Busca
                    <li>
                        <a href=@{CadastroR}>Cadastro

|]

widgetWelcome :: Widget
widgetWelcome = [whamlet|

    <div class="container">
        <div class="jumbotron">
            <h1>Geladeira Vazia
            <h2>Aqui você não passa fome!
            <h3>Feito por Cauê La Farina | Guilherme Egidio | Jorge Correa | Julliana Alvarez

|]


widgetHtmlHome :: Widget
widgetHtmlHome = [whamlet|
<h1>Geladeira Vazia
<h2>Aqui você não passa fome!
|]

widgetCss :: Widget
widgetCss = toWidget [lucius|
    h1{
       color:red;
    }
    h2{
       color:blue;
    }
|]

getHomeR :: Handler Html
getHomeR = defaultLayout (widgetImports >> widgetWelcome >> widgetCss)

getBuscaR :: Handler Html
getBuscaR = undefined

getListaR :: Handler Html
getListaR = undefined

getReceitaR :: Handler Html
getReceitaR = undefined

getCadastroR :: Handler Html
getCadastroR = undefined

-- GET POST cadastro de Ingredientes
getCadIngreR :: Handler Html
getCadIngreR = do
             (widget, enctype) <- generateFormPost formCadIngre
             defaultLayout $ widgetImports >> widgetForm CadIngreR enctype widget "Cadastro de Ingredientes"

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
             defaultLayout $ widgetImports >> widgetForm CadReceitaR enctype widget "Cadastro de Receitas"

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
             defaultLayout $ widgetImports >> widgetForm CadBuscaR enctype widget "Cadastro de Buscas"

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
             defaultLayout $ widgetForm CadCateR enctype widget "Cadastro de Categorias"

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
{-
getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ widgetForm CadastroR enctype widget "Pessoas"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid
             dpto <- runDB $ get $ pessoaDeptoid pessoa
             defaultLayout [whamlet|
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
                 <p> Depto: #{show $ fmap departamentoNome dpto}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaNome pessoa} <br>
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaNome pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR

getDeptoR :: Handler Html
getDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm DeptoR enctype widget "Departamentos"

postDeptoR :: Handler Html
postDeptoR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR

getUserR :: Handler Html
getUserR = do
             (widget, enctype) <- generateFormPost formUsuario
             defaultLayout $ widgetForm UserR enctype widget "Usuários"

postUserR :: Handler Html
postUserR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                       runDB $ insert usuario
                       defaultLayout [whamlet|
                           <h1> #{usuarioLogin usuario} Inserido com sucesso. 
                       |]
                    _ -> redirect UserR


getUsuarioR :: Handler Html
getUsuarioR = do
             listaP <- runDB $ selectList [] [Asc UsuarioLogin]
             defaultLayout [whamlet|
                 <h1> Usuarios cadastrados:
                 $forall Entity uid usuario <- listaP
                     <p> #{usuarioLogin usuario} <br>
             |]
-}
getAutR :: Handler Html
getAutR = do
          (widget, enctype) <- generateFormPost formUsuario
          defaultLayout $ widgetImports >> widgetForm AutR enctype widget "Login"

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
          defaultLayout [whamlet| BYE! <br>
                        <a href=@{HomeR}> Voltar|]

connStr = "dbname=ddniie89e3rtk3 host=ec2-54-225-199-108.compute-1.amazonaws.com user=haldvwbpgvjigm password=PQOXmc2BAD8zxzmBJmDx7KPMh6 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
--       warp 8080 $ Sitio pool
       warpEnv (Sitio pool)