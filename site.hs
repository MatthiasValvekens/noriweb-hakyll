--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkPandocM)

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteString.Lazy (toStrict)

import Control.Monad ((>=>), msum)

import qualified GHC.IO.Encoding as E

import qualified Data.Aeson as Aes
import Data.Scientific (toBoundedInteger)

--------------------------------------------------------------------------------

unminifiedCss :: Pattern
unminifiedCss = "static/css/*" .&&. complement "static/css/*.min.css"

simpleStaticAssets :: Pattern
simpleStaticAssets = ("static/**" .&&. complement unminifiedCss) .||. "robots.txt"

main :: IO ()
main = do
    -- because anything other than UTF-8 is ridiculous.
    E.setLocaleEncoding E.utf8
    hakyll hakyllRules

hakyllRules :: Rules ()
hakyllRules = do
    match simpleStaticAssets $ do
        route   idRoute
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        -- note: demoteHeaders decodes entities, which we don't want
        let ctx = snippetField <> defaultContext 
        compile $ getResourceBody >>= applyAsTemplate ctx

    match unminifiedCss $ do
        route idRoute
        compile compressCssCompiler

    match "contact.html" $ do
        route idRoute
        let ctx = constField "title" "Contact me" 
                <> snippetField
                <> copyrightContext
                <> defaultContext 

        compile $ getResourceBody >>= applyAsTemplate ctx 
                  >>= loadAndApplyTemplate "templates/default.html" ctx

    create ["profile.html"] $ do
        route idRoute 
        compile $ do 
            let aboutCtx = constField "title" "Bio"
                    <> field "extrastyle" (const $ loadBody "profile/style.css")
                    <> profileForLang "en"
                    <> profileForLang "ja"
                    <> copyrightContext
                    <> defaultContext
            makeItem ("" :: String) 
                >>= loadAndApplyTemplate "templates/profile.html" aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx

    -- Loosely based on https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("blog/**" .&&. hasNoVersion)
            specialPages <- loadAll (fromList ["profile.html", "contact.html", "blog.html"])
            let pages = specialPages ++ posts
            let rootCtx = constField "rootUrl" rootUrl
            let pgCtx = listField "pages" (rootCtx <> defaultContext) (return pages)
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/sitemap.xml" (rootCtx <> pgCtx)

    match "blog.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("blog/**" .&&. hasNoVersion)
            let ctx = listField "posts" postCtx (return posts)
                    <> copyrightContext
                    <> defaultContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "blog/**" $ do
        route $ gsubRoute "^blog/[0-9]+/" (const "blog/") `composeRoutes` setExtension ".html"
        let ctx = field "article-meta" jsonldMetaForItem <> postCtx
        compile $ pandocBlogPostCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx 
            >>= loadAndApplyTemplate "templates/default.html" ctx

    match "blog/**" $ version "jsonld-meta" $ do
        -- override the url field to point to the base version
        let ctx = field "abs-url" (absoluteUri . forBaseVer) 
                <> field "url" (routeOrFail . forBaseVer) <> postCtx
        compile $ makeItem ("" :: String)
            >>= loadAndApplyTemplate "templates/jsonld/article-info.json" ctx

    match "templates/**" $ compile templateBodyCompiler
    match "snippets/**" $ compile getResourceBody
    match "profile/**" $ compile getResourceBody

    where forBaseVer = setVersion Nothing . itemIdentifier
          profileForLang :: String -> Context String
          profileForLang lang = field name (const $ itemBody <$> rendered)
            where name = "profile-" <> lang
                  fname = fromFilePath $ "profile/profile-" <> lang <> ".md"
                  rendered = load fname >>= renderPandoc


--------------------------------------------------------------------------------

rootUrl :: String
rootUrl = "https://noriko.yakushiji.be"


getStringFromMeta :: String -> Identifier -> Compiler String
getStringFromMeta entryKey ident = do
    metadata <- getMetadata ident
    case lookupString entryKey metadata of
        Nothing -> noResult $ "No " ++ entryKey ++ " in metadata"
        Just value -> return value

fieldFromItemMeta :: String -> Context String
fieldFromItemMeta name = field name $ getStringFromMeta name . itemIdentifier


jsonldMetaFor :: Identifier -> Compiler String
jsonldMetaFor = loadBody . setVersion (Just "jsonld-meta")

jsonldMetaForItem :: Item a -> Compiler String
jsonldMetaForItem = jsonldMetaFor . itemIdentifier


routeOrFail :: Identifier -> Compiler String
routeOrFail ident = do
    maybeRoute <- getRoute ident
    case maybeRoute of
        Nothing -> fail $ "No route to '" ++ show ident ++ "'."
        Just x -> return ("/" ++ x)

absoluteUri :: Identifier -> Compiler String
absoluteUri = fmap (rootUrl++) . routeOrFail


postCtx :: Context String
postCtx = dateField "date" "%F" 
             <> fieldFromItemMeta "lang"
             <> copyrightContext
             <> defaultContext


mainAuthor :: String
mainAuthor = "Noriko Yakushiji"

getItemAuthor :: Item a -> Compiler (Maybe String)
getItemAuthor item = getMetadata (itemIdentifier item)
        >>= return . lookupString "author"


copyrightContext :: Context String
copyrightContext = field "author" (fmap checkAuthor . getItemAuthor)
        <> fieldFromItemMeta "author-url"
        <> field "copyrightline" (fmap cline . getItemAuthor)
    where cline Nothing = mainAuthor
          cline (Just author)
            | author == mainAuthor = mainAuthor
            | otherwise = author <> ", " <> mainAuthor
          checkAuthor Nothing = mainAuthor
          checkAuthor (Just author) = author

-------------------------------------------------
-- Pandoc stuff for blog posts


shiftAndStyleHeadings :: Int -> Block -> Block
shiftAndStyleHeadings by (Header lvl attr content) = Header lvl' attr' content
    where (elId, classes, kvals) = attr
          lvl' = lvl + by
          -- we up the level one more in Bulma styling
          classes' = ("subtitle":"is-" <> (T.pack $ show $ lvl' + 1):classes)
          attr' = (elId, classes', kvals)
shiftAndStyleHeadings _ x = x


grabJsonObj :: T.Text -> Compiler Aes.Object
grabJsonObj json = do
    case Aes.decodeStrict (encodeUtf8 json) of
        Just (Aes.Object jsonObj) -> return jsonObj
        Just _ -> fail "JSON type error: expected object"
        Nothing -> fail "JSON decoding failure"

jsonString :: Aes.ToJSON a => a -> String
jsonString = T.unpack . decodeUtf8 . toStrict . Aes.encode


formatInlineMetadata :: Block -> Compiler Block
formatInlineMetadata orig@(CodeBlock attr jsonMeta)
    | not ("meta" `elem` classes) = return orig
    | otherwise = do
        json <- grabJsonObj jsonMeta >>= insertDefault "@id" defaultId
        let ctx = constField "jsonld-meta" (jsonString json)
        metaItem <- makeItem ("" :: String) 
                        >>= loadAndApplyTemplate templateName ctx
        return $ RawBlock "html" $ T.pack $ itemBody metaItem
    where (elId, classes, _) = attr
          insertDefault k defaultVal = HMS.alterF alter k
            where alter Nothing = Just <$> defaultVal
                  alter (Just x) = return (Just x)
          templateName = "templates/jsonld/jsonld-meta.html"
          defaultId = do
            if elId == "" then fail "Inline metadata must have an ID" else pure ()
            currentUrl <- getUnderlying >>= routeOrFail
            return $ Aes.String (T.pack (rootUrl ++ currentUrl) <> "#" <> elId)
            

formatInlineMetadata orig = return orig


formatYoutubeFromMeta :: T.Text -> T.Text -> Compiler (Item String)
formatYoutubeFromMeta ytid jsonMeta = do
        rawObj <- grabJsonObj jsonMeta
        width <- extractIntOrFail "width" rawObj
        height <- extractIntOrFail "height" rawObj
        title <- extractStringOrFail "name" rawObj
        let videoUrl = "https://www.youtube.com/watch?v=" <> ytid
        let embedUrl = "https://www.youtube-nocookie.com/embed/" <> ytid
        let thumbnailUrl = "https://img.youtube.com/vi/" <> ytid <> "/maxresdefault.jpg"
        let contentUrl = "https://youtube.googleapis.com/v/" <> ytid
        let newObj = HMS.insert "embedUrl" (Aes.String embedUrl)
                   $ HMS.insert "thumbnailUrl" (Aes.String thumbnailUrl)
                   $ HMS.insert "contentUrl" (Aes.String contentUrl) rawObj
        let ctx = constField "width" (show width) <> constField "height" (show height)
                <> constField "embed-url" (T.unpack embedUrl)
                <> constField "video-url" (T.unpack videoUrl)
                <> constField "youtube-meta" (jsonString newObj)
                <> constField "title" title
        makeItem ("" :: String) >>= loadAndApplyTemplate templateName ctx
    where templateName = "templates/youtube-embed.html"
          extractIntOrFail :: T.Text -> Aes.Object -> Compiler Int
          extractIntOrFail key obj = do
            case HMS.lookup key obj of
                Just (Aes.Number x) -> case toBoundedInteger x of
                    Nothing -> fail "Expected int in JSON, got something else"
                    Just y -> return y
                _ -> fail $ "No numeric key " ++ T.unpack key ++ " in YouTube meta"
          extractStringOrFail key obj = do
            case HMS.lookup key obj of
                Just (Aes.String x) -> return (T.unpack x)
                _ -> fail $ "No string key " ++ T.unpack key ++ " in YouTube meta"


embedYoutubeVideos :: Block -> Compiler Block
embedYoutubeVideos orig@(CodeBlock attr jsonMeta)
    | not ("youtube" `elem` classes) = return orig
    | otherwise = do
        ytid <- extractPandocAttr "ytid" kvals
        ytEmbedItem <- formatYoutubeFromMeta ytid jsonMeta
        let ytContent = RawBlock "html" $ T.pack $ itemBody ytEmbedItem
        return $ Div (elId, classes, []) [ytContent]
    where (elId, classes, kvals) = attr

embedYoutubeVideos orig = return orig


extractPandocAttr :: T.Text -> [(T.Text, T.Text)] -> Compiler T.Text
extractPandocAttr key kvals = case msum (trans <$> kvals) of
        Nothing -> fail $ "No attribute value for '" ++ T.unpack key ++ "'."
        Just x -> return x
    where trans (k, v) = if k == key then (Just v) else Nothing


pandocBlogPostCompiler :: Compiler (Item String)
pandocBlogPostCompiler = getResourceBody >>= readPandoc >>= processPandoc
    where transform = walkPandocM $ return . shiftAndStyleHeadings 1 
                        >=> embedYoutubeVideos >=> formatInlineMetadata
          processPandoc = withItemBody transform >=> return . writePandoc
