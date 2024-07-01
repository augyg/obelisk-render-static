{-# LANGUAGE OverloadedStrings #-}

import Frontend (frontendStatic)
import FrontendUnconstrained -- your Frontend in Obelisk :: FrontendUnconstrained

import Reflex.Dom
import Control.Monad.IO.Class 
import qualified Data.Text as T
import qualified Data.ByteString as BS

-- | TODO: enumerate routes
main :: IO ()
main = do
  bs <- renderFrontendHtml (_frontendUnconstrained_head frontendStatic) (_frontendUnconstrained_body frontendStatic) (preloadGhcjs "all.js") (deferredGhcjsScript "all.js")
  BS.writeFile "index.html" bs


type FrontendWidgetT = HydratableT (PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost)))
renderFrontendHtml
  :: MonadIO m
  => FrontendWidgetT ()
  -> FrontendWidgetT ()
  -> FrontendWidgetT ()
  -> FrontendWidgetT ()
  -> m BS.ByteString
renderFrontendHtml frontendHead frontendBody headExtra bodyExtra = do
  --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
  html <- fmap snd $ liftIO $ renderStatic $ runHydratableT $ do
    el "html" $ do
      el "head" $ do
        frontendHead
        headExtra
      el "body" $ do
        frontendBody
        bodyExtra
  return $ "<!DOCTYPE html>" <> html
  

preloadGhcjs :: T.Text -> FrontendWidgetT ()
preloadGhcjs allJsUrl = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: allJsUrl) blank

-- | Load the script from the given URL in a deferred script tag.
-- This is the default method.
deferredGhcjsScript :: T.Text -> FrontendWidgetT ()
deferredGhcjsScript allJsUrl = elAttr "script" ("type" =: "text/javascript" <> "src" =: allJsUrl <> "defer" =: "defer") blank
