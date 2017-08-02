module Handler.Panel where

import Import
import Database.Persist.Sql

getPanelIndexR :: Handler Html
getPanelIndexR = do
    panels :: [Entity Panel] <- runDB $ selectList [] []
    defaultLayout $
        $(widgetFile "panel/index")

postPanelIndexR :: Handler Html
postPanelIndexR = do
    ((result, widget), enctype) <- runFormPost panelForm
    case result of
        FormSuccess panel -> do
            panelId <- runDB $ insert panel
            redirect $ PanelR panelId
            -- runDB (insert panel) >>= redirect . PanelR

        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PanelIndexR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                |]

getNewPanelR :: Handler Html
getNewPanelR = do
    (formW, enctype) <- generateFormPost panelForm
    defaultLayout $ 
        $(widgetFile "panel/new")

panelForm :: Form Panel
panelForm = renderDivs $
    Panel
        <$> areq textField "Title" Nothing
        <*> areq textField "Description" Nothing

getPanelR :: PanelId -> Handler Html
getPanelR panelId = do
    mPanel <- runDB $ get panelId
    case mPanel of
        Nothing -> 
            defaultLayout $ do
                let printablePanelId = fromSqlKey panelId
                [whamlet|
                    <p>Can't find panel with id #{printablePanelId}
                        |]
        Just panel ->
            defaultLayout
                [whamlet|
                    <h2>#{panelTitle panel}
                    <p>#{panelDescription panel}
                        |]

postPanelR :: PanelId -> Handler Html
postPanelR panelId = do
    undefined
