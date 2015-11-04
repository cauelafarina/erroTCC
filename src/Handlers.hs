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

formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident22",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","2")]} Nothing

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
              areq textField "Login" Nothing <*>
              areq textField "Senha" Nothing

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = [whamlet|
            <h1>
                Cadastro de #{y}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
|] >> toWidget [lucius|
       label{
          color:blue;
       }
|]

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

getAutR :: Handler Html
getAutR = do
          (widget, enctype) <- generateFormPost formUsuario
          defaultLayout $ widgetForm AutR enctype widget "Login"

postAutR :: Handler Html
postAutR = do
           ((result, _), _) <- runFormPost formUsuario
           case result of
                    FormSuccess user -> do
                       setSession "_ID" (usuarioLogin user)
                       redirect CadastroR
                    _ -> redirect AutR

getByeR :: Handler Html
getByeR = do
          deleteSession "_ID"
          defaultLayout [whamlet| BYE! |]

connStr = "dbname=d315s7h037m5g4 host=ec2-107-21-219-201.compute-1.amazonaws.com user=vlfhtvmuqtkxwb password=m_7hoITm4EhoCGrPJn_4Px7GGm port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warpEnv (Sitio pool)