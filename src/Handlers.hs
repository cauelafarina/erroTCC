{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT) --ver criação do banco
import Control.Applicative --criar formulario
import Data.Text
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes --dizer que o arquivo é só de handler

formCadIngre :: Form Ingrediente
formCadIngre = renderDivs $ Ingrediente <$>
              areq textField FieldSettings{
                        fsId = Just("nome"),
                        fsLabel = "Nome",
                        fsTooltip = Nothing,
                        fsName = Just ("nome"),
                        fsAttrs = [("placeholder","Nome"),("class","form-control")]
                    } Nothing

formCadReceita :: Form Receita
formCadReceita = renderDivs $ Receita <$>
              areq (selectField catg) FieldSettings{
                        fsId = Just("categoria"),
                        fsLabel = "Categoria",
                        fsTooltip = Nothing,
                        fsName = Just ("categoria"),
                        fsAttrs = [("placeholder","Categoria"),("class","form-control")]
                    } Nothing <*>
              areq textField FieldSettings{
                        fsId = Just("nome"),
                        fsLabel = "Nome",
                        fsTooltip = Nothing,
                        fsName = Just ("nome"),
                        fsAttrs = [("placeholder","Nome"),("class","form-control")]
                    } Nothing <*>
              areq textareaField FieldSettings{
                        fsId = Just("descricao"),
                        fsLabel = "Descrição",
                        fsTooltip = Nothing,
                        fsName = Just ("descricao"),
                        fsAttrs = [("placeholder","Descrição"),("class","form-control")]
                    } Nothing

catg = do
       entidades <- runDB $ selectList [] [Asc CategoriaNome]
       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades

formCadCateg :: Form Categoria
formCadCateg = renderDivs $ Categoria <$>
              areq textField FieldSettings{
                        fsId = Just("nome"),
                        fsLabel = "Nome",
                        fsTooltip = Nothing,
                        fsName = Just ("nome"),
                        fsAttrs = [("placeholder","Nome"),("class","form-control")]
                    } Nothing

formCadBusca :: Form Busca
formCadBusca = renderDivs $ Busca <$>
              areq (selectField rec) FieldSettings{
                        fsId = Just("receita"),
                        fsLabel = "Receita",
                        fsTooltip = Nothing,
                        fsName = Just ("receita"),
                        fsAttrs = [("placeholder","Receita"),("class","form-control")]
                    } Nothing <*>
              areq (selectField ing) FieldSettings{
                        fsId = Just("ingredientes"),
                        fsLabel = "Ingredientes",
                        fsTooltip = Nothing,
                        fsName = Just ("ingredientes"),
                        fsAttrs = [("placeholder","Ingredientes"),("class","form-control")]
                    } Nothing

rec = do
       entidades <- runDB $ selectList [] [Asc ReceitaNome]
       optionsPairs $ fmap (\ent -> (receitaNome $ entityVal ent, entityKey ent)) entidades

ing = do
       entidades <- runDB $ selectList [] [Asc IngredienteNome]
       optionsPairs $ Prelude.map (\ent -> (ingredienteNome $ entityVal ent, entityKey ent)) entidades

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
              areq textField FieldSettings{
                        fsId = Just("nome"),
                        fsLabel = "Nome",
                        fsTooltip = Nothing,
                        fsName = Just ("nome"),
                        fsAttrs = [("placeholder","Nome"),("class","form-control")]
                    } Nothing <*>

              areq passwordField FieldSettings{
                    fsId = Just("senha"),
                    fsLabel = "Senha",
                    fsTooltip = Nothing,
                    fsName = Just ("senha"),
                    fsAttrs = [("placeholder","Senha"),("class","form-control")]
                    } Nothing

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "hamlets/form.hamlet")
     toWidget $(luciusFile "lucius/teste.lucius")

wHead :: String -> Widget
wHead title = toWidgetHead [hamlet|
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href=@{StaticR css_bootstrap_min_css} rel="stylesheet"/>
    <link href=@{StaticR css_business_casual_css} rel="stylesheet"/>
|]

wNavigation :: Widget
wNavigation = [whamlet|
        <div class="brand">Do Armário à Geladeira
        <div class="address-bar">Aqui você não passa fome

        <!-- Navigation -->
        <nav class="navbar navbar-default" role="navigation">
            <div class="container">
                <!-- Brand and toggle get grouped for better mobile display -->
                <div class="navbar-header">
                    <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
                        <span class="sr-only">Toggle navigation
                        <span class="icon-bar">
                        <span class="icon-bar">
                        <span class="icon-bar">
                    <!-- navbar-brand is hidden on larger screens, but visible when the menu is collapsed -->
                    <a class="navbar-brand" href=@{HomeR}>Home
                <!-- Collect the nav links, forms, and other content for toggling -->
                <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                    <ul class="nav navbar-nav">
                        <li>
                            <a href=@{HomeR}>Home
                        <li class="dropdown">
                            <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">
                              Listar
                              <span class="caret">
                               <ul class="dropdown-menu">
                                 <li>
                                   <a href=@{ListCateR}>Categorias
                                 <li>
                                   <a href=@{ListIngreR}>Ingredientes
                                 <li>
                                   <a href=@{ListReceitaR}>Receitas
                        <li>
                            <a href=@{CadastroR}>Cadastro
                        <li>
                            <a href=@{CreditoR}>Sobre
                        <li>
                            <a href=@{ByeR}>Sair
                <!-- /.navbar-collapse -->
|]

wFooter :: Widget
wFooter = [whamlet|
    <footer>
        <div class="container">
            <div class="row">
                <div class="col-lg-12 text-center">
                    <p>Copyright &copy; Your Website 2015
|]

wContainer :: String -> Widget -> Widget
wContainer title content = do
    wHead title
    [whamlet|
    <div class="container">
         ^{wNavigation}
         ^{content}
         ^{wFooter}
|]

widgetWelcome :: Widget
widgetWelcome = [whamlet|
        <div class="container">

          <div class="row">
            <div class="box">
                <div class="col-lg-12 text-center">
                    <div id="carousel-example-generic" class="carousel slide">
                        <!-- Indicators -->
                        <ol class="carousel-indicators hidden-xs">
                            <li data-target="#carousel-example-generic" data-slide-to="0" class="active">
                            <li data-target="#carousel-example-generic" data-slide-to="1">
                            <li data-target="#carousel-example-generic" data-slide-to="2">

                        <!-- Wrapper for slides -->
                        <div class="carousel-inner">
                            <div class="item active">
                                <img class="img-responsive img-full" src=@{StaticR img_slide1_jpg} alt="">
                            <div class="item">
                                <img class="img-responsive img-full" src=@{StaticR img_slide2_jpg} alt="">
                            <div class="item">
                                <img class="img-responsive img-full" src=@{StaticR img_slide3_jpg} alt="">

                        <!-- Controls -->
                        <a class="left carousel-control" href="#carousel-example-generic" data-slide="prev">
                            <span class="icon-prev">
                        <a class="right carousel-control" href="#carousel-example-generic" data-slide="next">
                            <span class="icon-next">
                    <h2 class="brand-before">
                        <small>Bem vindo à
                    <h1 class="brand-name">Do Armário à Geladeira
                    <hr class="tagline-divider">
                    <h2>
                        <small>by
                            <strong>Start Bootstrap
|]

wCadastro :: Widget
wCadastro = do [whamlet|
    <div class="row">
       <div class="box" >
          <div class="col-xs-6 col-md-3">
            <a href=@{CadIngreR} class="thumbnail">
              <h4>Ingredientes
          <div class="col-xs-6 col-md-3">
            <a href=@{CadCateR} class="thumbnail">
              <h4>Categorias
          <div class="col-xs-6 col-md-3">
            <a href=@{CadReceitaR} class="thumbnail">
              <h4>Receitas
          <div class="col-xs-6 col-md-3">
            <a href=@{CadBuscaR} class="thumbnail">
              <h4>Buscas
|]

wCredito :: Widget
wCredito = do
    [whamlet|
    <div class="container">

        <div class="row">
            <div class="box">
                <div class="col-lg-12">
                    <hr>
                    <h2 class="intro-text text-center">Proposta
                        <strong>DoArmarioaGeladeira
                    <hr>
                <div class="col-md-6">
                    <img class="img-responsive img-border-left" src=@{StaticR img_logo_jpg} alt="">
                <div class="col-md-6">
                    <p>Esse foi um projeto de finalização de curso de Análise e Desenvolimento de Sistemas.
                    <p>Nossa proposta foi construir uma plataforma culinária com um diferencial na maneira de buscar as receitas.
                    <p>Aqui você entra com os ingredientes que você encontra na sua cozinha e nós retornamos as possíveis receitas à serem feitas com eles.
                <div class="clearfix">

        <div class="row">
            <div class="box">
                <div class="col-lg-12">
                    <hr>
                    <h2 class="intro-text text-center">Our
                        <strong>Team
                    <hr>
                <div class="col-sm-3 text-center">
                    <img class="img-responsive" src=@{StaticR img_caue_jpg} alt="">
                    <h4>Caue La Farina<br>
                        <small>1310007-7
                <div class="col-sm-3 text-center">
                    <img class="img-responsive" src=@{StaticR img_guilherme_jpg} alt="">
                    <h4>Guilherme Egidio<br>
                        <small>R.A.
                <div class="col-sm-3 text-center">
                    <img class="img-responsive" src=@{StaticR img_jorge_jpg} alt="">
                    <h4>Jorge Correa<br>
                        <small>R.A.
                <div class="col-sm-3 text-center">
                    <img class="img-responsive" src=@{StaticR img_julliana_jpg} alt="">
                    <h4>Julliana Alvarez<br>
                        <small>R.A.
                <div class="clearfix">
    <!-- /.container -->
|]

wListReceitas :: Widget
wListReceitas = do
    listaR <- handlerToWidget $ runDB $ selectList [] [Asc ReceitaNome]
    [whamlet|
    <div class="container">
         <div class="row">
              <div class="box">
                   <div class="col-lg-12 text-center">
                        <h1>
                            Lista de Receitas
         <div class="row">
              <div class="box">
                   <div class="col-md-12">
                        <div class="table-responsive">
                             <table class="table table-hover">
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
    <div class="container">
        <div class="row">
            <div class="box">
                <div class="col-lg-12">
                    <hr>
                    <h2 class="intro-text text-center">
                        <strong>#{receitaNome receita}
                    <hr>
                <div class="col-lg-12 text-center">
                    <img class="img-responsive img-border img-full" src=@{StaticR img_receita1_jpg} alt="">
                    <h2>Como fazer:
                        <br>
                        <small>
                    <p>#{receitaDescricao receita}
|]

wListIngre :: Widget
wListIngre = do
    listaI <- handlerToWidget $ runDB $ selectList [] [Asc IngredienteNome]
    [whamlet|
    <div class="container">
         <div class="row">
              <div class="box">
                   <div class="col-lg-12 text-center">
                        <h1>
                            Lista de Ingredientes
         <div class="row">
              <div class="box">
                   <div class="col-md-12">
                        <div class="table-responsive">
                             <table class="table table-hover">
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
    <div class="container">
         <div class="row">
              <div class="box">
                   <div class="col-lg-12 text-center">
                        <h1>
                            Lista de Categorias
         <div class="row">
              <div class="box">
                   <div class="col-md-12">
                        <div class="table-responsive">
                             <table class="table table-hover">
                                    <thead>
                                           <tr>
                                               <th>Categorias Cadastradas
                                    <tbody>
                                        $forall Entity cid categoria <- listaC
                                           <tr>
                                               <td>
                                                   <a href=@{CategoriaR cid}>#{categoriaNome categoria}
|]

wCategoria :: CategoriaId -> Widget
wCategoria cid = do
    listaR <- handlerToWidget $ runDB $ selectList [ReceitaCatid ==. cid] [Asc ReceitaNome]
    [whamlet|
    <div class="container">
         <div class="row">
              <div class="box">
                   <div class="col-lg-12 text-center">
                        <h1>
                            Lista de Receitas
         <div class="row">
              <div class="box">
                   <div class="col-md-12">
                        <div class="table-responsive">
                             <table class="table table-hover">
                                    <thead>
                                           <tr>
                                               <th>Receitas Cadastrados
                                    <tbody>
                                        $forall Entity rid receita <- listaR
                                           <tr>
                                               <td>
                                                   <a href=@{ReceitaR rid}>#{receitaNome receita}
|]


getHomeR :: Handler Html
getHomeR = defaultLayout $ (wContainer "Home" widgetWelcome) >> toWidget $(luciusFile "lucius/boot.lucius")

getBuscaR :: Handler Html
getBuscaR = undefined

getListaR :: Handler Html
getListaR = undefined

getCadastroR :: Handler Html
getCadastroR = defaultLayout $ (wContainer "Créditos" wCadastro) >> toWidget $(luciusFile "lucius/boot.lucius")

getCreditoR :: Handler Html
getCreditoR = defaultLayout $ (wContainer "Créditos" wCredito) >> toWidget $(luciusFile "lucius/boot.lucius")

-- GET POST cadastro de Ingredientes
getCadIngreR :: Handler Html
getCadIngreR = do
             (widget, enctype) <- generateFormPost formCadIngre
             defaultLayout $ (wContainer "Cadastro de Ingredientes" (widgetForm CadIngreR enctype widget "Cadastro de Ingredientes" "Cadastrar")) >> toWidget $(luciusFile "lucius/boot.lucius")


postCadIngreR :: Handler Html
postCadIngreR = do
                ((result, _), _) <- runFormPost formCadIngre
                case result of
                    FormSuccess ingre -> do
                       runDB $ insert ingre
                       setMessage $ [shamlet| <p> #{ingredienteNome ingre} inserido com sucesso. |]
                       redirect CadIngreR
                    _ -> redirect CadIngreR
-- GET POST cadastro de Ingredientes

-- GET POST cadastro de Receitas
getCadReceitaR :: Handler Html
getCadReceitaR = do
             (widget, enctype) <- generateFormPost formCadReceita
             defaultLayout $ (wContainer "Cadastro de Receitas" (widgetForm CadReceitaR enctype widget "Cadastro de Receitas" "Cadastrar")) >> toWidget $(luciusFile "lucius/boot.lucius")

postCadReceitaR :: Handler Html
postCadReceitaR = do
                ((result, _), _) <- runFormPost formCadReceita
                case result of
                    FormSuccess receita -> do
                       runDB $ insert receita
                       setMessage $ [shamlet| <p> #{receitaNome receita} inserido com sucesso. |]
                       redirect CadReceitaR
                    _ -> redirect CadReceitaR
-- GET POST cadastro de Receitas

-- GET POST cadastro de Busca
getCadBuscaR :: Handler Html
getCadBuscaR = do
             (widget, enctype) <- generateFormPost formCadBusca
             defaultLayout $ (wContainer "Cadastro de Buscas" (widgetForm CadBuscaR enctype widget "Cadastro de Buscas" "Cadastrar")) >> toWidget $(luciusFile "lucius/boot.lucius")

postCadBuscaR :: Handler Html
postCadBuscaR = do
                ((result, _), _) <- runFormPost formCadBusca
                case result of
                    FormSuccess busca -> do
                       runDB $ insert busca
                       setMessage $ [shamlet| <p> Busca inserida com sucesso. |]
                       redirect CadBuscaR
                    _ -> redirect CadBuscaR
-- GET POST cadastro de Busca


-- GET POST cadastro de Categorias
getCadCateR :: Handler Html
getCadCateR = do
             (widget, enctype) <- generateFormPost formCadCateg
             defaultLayout $ (wContainer "Cadastro de Categorias" (widgetForm CadCateR enctype widget "Cadastro de Categorias" "Cadastrar")) >> toWidget $(luciusFile "lucius/boot.lucius")

postCadCateR :: Handler Html
postCadCateR = do
                ((result, _), _) <- runFormPost formCadCateg
                case result of
                    FormSuccess categoria -> do
                       runDB $ insert categoria
                       setMessage $ [shamlet| <p> #{categoriaNome categoria} inserido com sucesso. |]
                       redirect CadCateR
                    _ -> redirect CadCateR
-- GET POST cadastro de Categorias

-- GET POST cadastro de Usuario
getCadUseR :: Handler Html
getCadUseR = do
             (widget, enctype) <- generateFormPost formUsuario
             defaultLayout $ (wContainer "Cadastro de Usuarios" (widgetForm CadUseR enctype widget "Cadastro de Usuários" "Cadastrar")) >> toWidget $(luciusFile "lucius/boot.lucius")

postCadUseR :: Handler Html
postCadUseR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                       runDB $ insert usuario
                       setMessage $ [shamlet| <p> #{usuarioNome usuario} inserido com sucesso. |]
                       redirect CadUseR
                    _ -> redirect CadUseR
-- GET POST cadastro de Usuario

getListIngreR :: Handler Html
getListIngreR = defaultLayout $ (wContainer "Lista de Ingredientes" wListIngre) >> toWidget $(luciusFile "lucius/boot.lucius")

getListReceitaR :: Handler Html
getListReceitaR = defaultLayout $ (wContainer "Lista de Receitas" wListReceitas) >> toWidget $(luciusFile "lucius/boot.lucius")

getReceitaR :: ReceitaId -> Handler Html
getReceitaR rid = defaultLayout $ (wContainer "Info Receita" (wReceita rid)) >> toWidget $(luciusFile "lucius/boot.lucius")

getCategoriaR :: CategoriaId -> Handler Html
getCategoriaR cid = defaultLayout $ (wContainer "Info Categoria" (wCategoria cid)) >> toWidget $(luciusFile "lucius/boot.lucius")

getListCateR :: Handler Html
getListCateR = defaultLayout $ (wContainer "Lista de Categorias" wListCate) >> toWidget $(luciusFile "lucius/boot.lucius")

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsuario
    defaultLayout $ (wContainer "Login" (widgetForm LoginR enc wid "" "Log in")) >> toWidget $(luciusFile "lucius/boot.lucius")

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioPass ==. usuarioPass usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect CadastroR
                Nothing -> do
                    setMessage $ [shamlet| <p>Usuário inválido |]
                    redirect LoginR
        _ -> redirect LoginR

getByeR :: Handler Html
getByeR = do
          deleteSession "_ID"
          defaultLayout $ (wContainer "Login" ([whamlet| <h1> BYE! <br>
                        <a href=@{HomeR}> Voltar|])) >> toWidget $(luciusFile "lucius/boot.lucius")

connStr = "dbname=d6fj7u9j3cc8jn host=ec2-107-21-221-107.compute-1.amazonaws.com user=gcpykscolfkpbo password=8uEXiyfq8JCR0YNng9IAcFgDEV port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
--       warp 8080 $ Sitio pool s
       warpEnv (Sitio pool s)