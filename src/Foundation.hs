{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Ingrediente
   nome Text
   deriving Show

Categoria
   nome Text
   deriving Show

Receita
   catid CategoriaId
   nome Text
   descricao Textarea
   deriving Show

Busca
   recid ReceitaId
   ingid IngredienteId
   deriving Show

Usuario
   nome Text
   pass Text
   deriving Show
|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ LoginR --quando precisar se autenticar
    isAuthorized CadastroR _ = isAdmin --torna a rota necessaria por login
    isAuthorized CadIngreR _ = isAdmin
    isAuthorized CadReceitaR _ = isAdmin
    isAuthorized CadBuscaR _ = isAdmin
    isAuthorized CadCateR _ = isAdmin
    isAuthorized _ _ = return Authorized

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Voce tem que ser um administrador"

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

