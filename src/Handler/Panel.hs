module Handler.Panel where

import           Import

import           Database.Persist.Sql
import           Yesod.Form.Bootstrap3

getPanelIndexR :: Handler Html
getPanelIndexR = do
    panels :: [Entity Panel] <- runDB $ selectList [] []
    defaultLayout $
        $(widgetFile "panel/index")

postPanelIndexR :: Handler Html
postPanelIndexR = do
    ((result, formW), enctype) <- runFormPost panelForm
    case result of
        FormSuccess panel ->
            redirect . PanelR =<< runDB (insert panel)
        _ ->
            renderForm formW enctype

getNewPanelR :: Handler Html
getNewPanelR = do
    (formW, enctype) <- generateFormPost panelForm
    renderForm formW enctype

renderForm :: Widget -> Enctype -> Handler Html
renderForm formW enctype =
    defaultLayout $ do
        setTitle "New Panel"
        $(widgetFile "panel/new")

panelForm :: Form Panel
panelForm = renderBootstrap3 BootstrapBasicForm $
    Panel
        <$> areq textField (bfs ("Title" :: Text)) Nothing
        <*> areq textField (bfs ("Description" :: Text)) Nothing

getPanelR :: PanelId -> Handler Html
getPanelR panelId = do
    panel <- runDB $ get404 panelId
    muserId <- maybeAuthId
    questionModels <- runDB $ selectList [QuestionPanel ==. panelId] []
    questions <- for questionModels $ \qmodel -> do
        votes <- runDB $ selectList [VoteQuestion ==. entityKey qmodel] []
        pure (renderQuestion qmodel (map entityVal votes) muserId)
    (qform, enctype) <- generateFormPost questionForm
    defaultLayout $(widgetFile "panel/show")

renderQuestion :: Entity Question -> [Vote] -> Maybe UserId -> Widget
renderQuestion (Entity questionId question) votes muserId =
    $(widgetFile "question")
  where
    (upvotes, downvotes) =
        bimap length length (partition voteApprove votes)
    userApproved = do
        userId <- muserId
        vote <- find (\v -> voteVoter v == userId) votes
        pure (voteApprove vote)

questionForm :: Form (UserId -> PanelId -> Question)
questionForm = renderBootstrap3 BootstrapBasicForm $
    Question
        <$> areq textField (bfs ("What to ask?" :: Text)) Nothing

postQuestionR :: PanelId -> Handler Html
postQuestionR panelId = do
    mid <- maybeAuthId
    userId <- maybe (redirect (AuthR LoginR)) pure mid
    ((result, _), _) <- runFormPost questionForm
    case result of
        FormSuccess mkQuestion -> do
            void . runDB $ insert (mkQuestion userId panelId)
            redirect (PanelR panelId)
        _ -> do
            setMessage "The question didn't save. Sorry!"
            redirect (PanelR panelId)

postUpVoteR :: QuestionId -> Handler Html
postUpVoteR = handleVote True

postDownVoteR :: QuestionId -> Handler Html
postDownVoteR = handleVote False

handleVote :: Bool -> QuestionId -> Handler Html
handleVote approve questionId = do
    mid <- maybeAuthId
    userId <- maybe (redirect (AuthR LoginR)) pure mid
    _ <- runDB $ upsert (Vote questionId userId approve) [VoteApprove =. approve]
    panelId <- fmap questionPanel . runDB $ get404 questionId
    redirect (PanelR panelId)
