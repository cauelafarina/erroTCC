{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Application where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT) --ver criação do banco
import Control.Applicative --criar formulario
import Data.Text
import Text.Lucius
import Prelude

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes --dizer que o arquivo é só de handler

--forms
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
--forms

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "hamlets/form.hamlet")
     toWidget $(luciusFile "lucius/imputs.lucius")

wHead :: String -> Widget
wHead title = toWidgetHead [hamlet|
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link href=@{StaticR css_bootstrap_min_css} rel="stylesheet"/>
    <link href=@{StaticR css_business_casual_css} rel="stylesheet"/>
    <link href=@{StaticR lucius_font_lucius} rel="stylesheet" type="text/css"/>
|]

--home
wHome :: Widget
wHome = do
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/home.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Home"
           wHome >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--home

--creditos
wCredito :: Widget
wCredito = do
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/creditos.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getCreditoR :: Handler Html
getCreditoR = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Créditos"
           wCredito >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--creditos

--listarReceitas
wListReceitas :: Widget
wListReceitas = do
    listaR <- handlerToWidget $ runDB $ selectList [] [Asc ReceitaNome]
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/listareceitas.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getListReceitaR :: Handler Html
getListReceitaR = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Lista de Receitas"
           wListReceitas >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--listarReceitas

--listarIngredientes
wListIngre :: Widget
wListIngre = do
    listaI <- handlerToWidget $ runDB $ selectList [] [Asc IngredienteNome]
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/listaingredientes.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getListIngreR :: Handler Html
getListIngreR = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Lista de Ingredientes"
           wListIngre >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--listarIngredientes

--listarCategorias
wListCate :: Widget
wListCate = do
    listaC <- handlerToWidget $ runDB $ selectList [] [Asc CategoriaNome]
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/listacategoria.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getListCateR :: Handler Html
getListCateR = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Lista de Categorias"
           wListCate >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--listarCategorias

--receita
wReceita :: ReceitaId -> Widget
wReceita rid = do
    receita <- handlerToWidget $ runDB $ get404 rid
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/receita.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getReceitaR :: ReceitaId -> Handler Html
getReceitaR rid = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Receitas"
           wReceita rid >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--receita

--categoria
wCategoria :: CategoriaId -> Widget
wCategoria cid = do
    listaR <- handlerToWidget $ runDB $ selectList [ReceitaCatid ==. cid] [Asc ReceitaNome]
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/categoria.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getCategoriaR :: CategoriaId -> Handler Html
getCategoriaR cid = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Categorias"
           wCategoria cid >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--categoria

getBuscaR :: Handler Html
getBuscaR = undefined

getListaR :: Handler Html
getListaR = undefined
wCadastro :: Widget

--cadastro
wCadastro = do
    wHead ""
    $(whamletFile "hamlets/nav.hamlet")
    $(whamletFile "hamlets/cadastro.hamlet")
    $(whamletFile "hamlets/footer.hamlet")

getCadastroR :: Handler Html
getCadastroR = defaultLayout $ do
           setTitle "Do Armário à Geladeira - Cadastro"
           wCadastro >> toWidget $(luciusFile "lucius/boot.lucius") >> toWidget $(luciusFile "lucius/font.lucius")
--cadastro

-- GET POST cadastro de Ingredientes
getCadIngreR :: Handler Html
getCadIngreR = do
             (widget, enctype) <- generateFormPost formCadIngre
             defaultLayout $ widgetForm CadIngreR enctype widget "Cadastro de Ingredientes" "Cadastrar" >> toWidget $(luciusFile "lucius/boot.lucius")


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
             defaultLayout $ widgetForm CadReceitaR enctype widget "Cadastro de Receitas" "Cadastrar" >> toWidget $(luciusFile "lucius/boot.lucius")

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
             defaultLayout $ widgetForm CadBuscaR enctype widget "Cadastro de Buscas" "Cadastrar" >> toWidget $(luciusFile "lucius/boot.lucius")

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
             defaultLayout $ widgetForm CadCateR enctype widget "Cadastro de Categorias" "Cadastrar" >> toWidget $(luciusFile "lucius/boot.lucius")

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
             defaultLayout $ widgetForm CadUseR enctype widget "Cadastro de Usuários" "Cadastrar" >> toWidget $(luciusFile "lucius/boot.lucius")

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

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsuario
    defaultLayout $ widgetForm LoginR enc wid "" "Log in" >> toWidget $(luciusFile "lucius/boot.lucius") 

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
          defaultLayout $ [whamlet| <h1> BYE! <br>
                        <a href=@{HomeR}> Voltar|] >> toWidget $(luciusFile "lucius/boot.lucius")

connStr = "dbname=tcc host=localhost user=tcc password=tcc port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warp 8080 $ Sitio pool s
--       warpEnv (Sitio pool s)