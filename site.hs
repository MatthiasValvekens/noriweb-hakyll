--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Hakyll

import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkPandocM)
import Text.Pandoc (writeMarkdown, runPure)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HMS
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Data.ByteString.Lazy (toStrict)

import Control.Monad ((>=>), msum, liftM)

import qualified GHC.IO.Encoding as E

import qualified Data.Aeson as Aes


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

    match "profile.html" $ do
        route idRoute 
        compile $ do 
            let aboutCtx = constField "title" "Bio"
                    <> field "extrastyle" (const $ loadBody "profile/style.css")
                    <> profileForLang "en"
                    <> profileForLang "ja"
                    <> copyrightContext
                    <> defaultContext
            getResourceBody
                >>= applyAsTemplate aboutCtx
                >>= loadAndApplyTemplate "templates/default.html" aboutCtx

    -- Loosely based on https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("blog/**/*.md" .&&. hasNoVersion)
            let spIdents = ["profile.html", "contact.html", "blog.html", "media.html"]
            specialPages <- loadAll (fromList spIdents)
            -- load page/media list pages as well
            indexes <- loadAll "**/pagelist/**"
            let pages = specialPages ++ indexes ++ posts
            let rootCtx = constField "rootUrl" rootUrl
            let pgCtx = listField "pages" (rootCtx <> defaultContext) (return pages)
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/sitemap.xml" (rootCtx <> pgCtx)

    blogPagination <- do
        let grp = liftM (paginateEvery 5) . sortRecentFirst
        buildPaginateWith grp ("blog/**/*.md" .&&. hasNoVersion) blogPageId

    paginateRules blogPagination $ \page pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let postInListCtx = postCtx <> field "preview" getPreview
            let ctx = constField "title" "Blog"
                    <> listField "posts" postInListCtx (return posts)
                    <> radialPaginationContext 2 blogPagination page
                    <> copyrightContext
                    <> defaultContext
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    mediaPagination <- do
        let grp = liftM (paginateEvery 3) . sortRecentFirst
        buildPaginateWith grp ("media/**/*.md") mediaPageId

    paginateRules mediaPagination $ \page pattern -> do
        route idRoute
        compile $ do
            mediaItems <- recentFirst =<< loadAll pattern
            let ctx = constField "title" "Media"
                    <> listField "media-items" defaultContext (return mediaItems)
                    <> radialPaginationContext 2 mediaPagination page
                    <> field "extrastyle" (const $ loadBody "media/media.css")
                    <> copyrightContext
                    <> defaultContext
            makeItem ("" :: String)
                >>= loadAndApplyTemplate "templates/media-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "media/media.css" $ compile getResourceBody

    match "blog/**/*.md" $ do
        route $ gsubRoute "^blog/[0-9]+/" (const "blog/post/") `composeRoutes` setExtension ".html"
        let ctx = field "article-meta" jsonldMetaForItem <> postCtx
        compile $ pandocBlogPostCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx 
            >>= loadAndApplyTemplate "templates/default.html" ctx

    match "blog/**/*.md" $ version "jsonld-meta" $ do
        -- override the url field to point to the base version
        let ctx = field "abs-url" (absoluteUri . forBaseVer) 
                <> field "url" (routeOrFail . forBaseVer) <> postCtx
        compile $ makeItem ("" :: String)
            >>= loadAndApplyTemplate "templates/jsonld/article-info.json" ctx

    match "repertoire.md" $ do
        route (setExtension ".html")
        compile $ do
            let ctx = field "extrastyle" (const $ loadBody "snippets/repertoire.css")
                    <> copyrightContext <> defaultContext
            genericPageCompiler 
                >>= loadAndApplyTemplate "templates/generic.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "blog/**/*.md" $ version "preview" $ compile (pandocPreviewCompiler)

    match "templates/**" $ compile templateBodyCompiler
    match "snippets/**" $ compile getResourceBody
    match "profile/**" $ compile getResourceBody
    match "media/soundcloud/*.md" $ compile compileSoundcloudMedia
    match "media/youtube/*.md" $ compile compileYoutubeMedia

    where forBaseVer = setVersion Nothing . itemIdentifier
          profileForLang :: String -> Context String
          profileForLang lang = field name (const $ itemBody <$> rendered)
            where name = "profile-" <> lang
                  fname = fromFilePath $ "profile/profile-" <> lang <> ".md"
                  rendered = load fname >>= renderPandoc

          blogPageId :: PageNumber -> Identifier
          blogPageId 1 = "blog.html"
          blogPageId n = fromFilePath $ "blog/pagelist/" ++ show n ++ ".html"

          mediaPageId :: PageNumber -> Identifier
          mediaPageId 1 = "media.html"
          mediaPageId n = fromFilePath $ "media/pagelist/" ++ show n ++ ".html"

          getPreview = loadBody . setVersion (Just "preview") . itemIdentifier


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

getItemAuthorUrl :: Item a -> Compiler (Maybe String)
getItemAuthorUrl item = getMetadata (itemIdentifier item)
        >>= return . lookupString "author-url"

copyrightContext :: Context String
copyrightContext = field "author" (fmap checkAuthor . getItemAuthor)
        <> field "author-url" (getItemAuthorUrl >=> checkAuthorUrl)
        <> field "copyrightline" (fmap cline . getItemAuthor)
    where cline Nothing = mainAuthor
          cline (Just author)
            | author == mainAuthor = mainAuthor
            | otherwise = author <> ", " <> mainAuthor
          checkAuthor Nothing = mainAuthor
          checkAuthor (Just author) = author
          checkAuthorUrl Nothing = do
            maybeRoute <- getRoute "profile.html"
            case maybeRoute of
                Nothing -> fail "no main author URL"
                Just rt -> return (rootUrl <> toUrl rt)
          checkAuthorUrl (Just authorUrl) = return authorUrl


data PageNumInfo = PageNumInfo 
                 { pniGetPageNum :: PageNumber
                 , pniGetPageUrl :: String }


radialPaginationContext :: PageNumber -> Paginate -> PageNumber -> Context a
radialPaginationContext rad p curPage = paginateContext p curPage 
                                      <> before <> after
    where -- for each number, build the URL to the page using getRoute
          pgNumItem :: PageNumber -> Compiler (Item PageNumInfo)
          pgNumItem n = do
            maybeRoute <- getRoute (paginateMakeId p n)
            case maybeRoute of
                Nothing -> fail $ "Couldn't retrieve URL for page " ++ show n
                Just rt -> makeItem $ PageNumInfo n (toUrl rt)

          -- turn a PageNumInfo into template fields
          pgNumCtx :: Context PageNumInfo
          pgNumCtx = field "pageNum" fmtPgNum <> field "pageUrl" fmtPgUrl
            where fmtPgNum = return . show . pniGetPageNum . itemBody
                  fmtPgUrl = return . pniGetPageUrl . itemBody
          lastPageNum = M.size $ paginateMap p
          seqOrNoResult [] = noResult "No pages"
          seqOrNoResult xs = sequence xs
          before = pgsField <> elide
            where pgsField = listField "pagesBefore" pgNumCtx (seqOrNoResult pgs)
                  fstInSet = max 2 (curPage - rad)
                  elide = field "elideBefore" $ const $ do
                    case fstInSet > 2 && not (null pgs) of
                        True -> return "elide"
                        False -> noResult "no ellipsis"
                  pgs = [pgNumItem n | n <- [fstInSet .. curPage - 1]]
          after = pgsField <> elide
            where pgsField = listField "pagesAfter" pgNumCtx (seqOrNoResult pgs)
                  lastInSet = min (lastPageNum - 1) (curPage + rad)
                  elide = field "elideAfter" $ const $ do
                    case lastInSet < lastPageNum - 1 && not (null pgs) of
                        True -> return "elide"
                        False -> noResult "no ellipsis"
                  pgs = [pgNumItem n | n <- [curPage + 1 .. lastInSet]]


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


extractStringOrFail :: T.Text -> Aes.Object -> Compiler String
extractStringOrFail key obj = case HMS.lookup key obj of
        Just (Aes.String x) -> return (T.unpack x)
        _ -> fail $ "No string key " ++ T.unpack key ++ " in JSON meta"


data Embedded = Embedded T.Text T.Text [Block] String (Maybe T.Text)
            
descrTextM :: MonadFail m => [Block] -> T.Text -> m T.Text
descrTextM descr title = case descr of
      [] -> return title
      _ -> case runPure (writeMarkdown defaultHakyllWriterOptions descrPandoc) of
          Right x -> return x 
          Left _ -> fail "Failed to compile description to markdown"
    where descrPandoc = Pandoc (Meta mempty) descr

descrAsHtml :: [Block] -> String
descrAsHtml descr = itemBody $ writePandoc $ Item "" descrPandoc
    where descrPandoc = Pandoc (Meta mempty) descr


formatYoutubeFromMeta :: Embedded -> T.Text -> Compiler (Item String)
formatYoutubeFromMeta (Embedded ytid name descr captionStyle mbUploadDate) jsonMeta = do
        rawObj <- grabJsonObj jsonMeta
        let width = 560
        let height = 315
        title <- if T.null name
                 then extractStringOrFail "name" rawObj
                 else return (T.unpack name)
        let videoUrl = "https://www.youtube.com/watch?v=" <> ytid
        let embedUrl = "https://www.youtube.com/embed/" <> ytid
        let thumbnailUrl = "https://img.youtube.com/vi/" <> ytid <> "/maxresdefault.jpg"
        let contentUrl = "https://youtube.googleapis.com/v/" <> ytid
        -- only override name/description if actually provided
        let nameIns = if T.null name
                      then id
                      else HMS.insert "name" (Aes.String name)
        let uplDateInsert = case mbUploadDate of
                Nothing -> id
                Just dt -> HMS.insert "uploadDate" (Aes.String dt)
        descrText <- descrTextM descr (T.pack title)
        let newObj = nameIns $ uplDateInsert
                   $ HMS.insert "width" (Aes.Number width)
                   $ HMS.insert "height" (Aes.Number height)
                   $ HMS.insert "description" (Aes.String descrText)
                   $ HMS.insert "name" (Aes.String $ T.pack title)
                   $ HMS.insert "url" (Aes.String videoUrl)
                   $ HMS.insert "embedUrl" (Aes.String embedUrl)
                   $ HMS.insert "thumbnailUrl" (Aes.String thumbnailUrl)
                   $ HMS.insert "contentUrl" (Aes.String contentUrl) rawObj
        let ctx = constField "width" (show width) <> constField "height" (show height)
                <> constField "embed-url" (T.unpack embedUrl)
                <> constField "video-url" (T.unpack videoUrl)
                <> constField "youtube-meta" (jsonString newObj)
                <> constField "caption" (descrAsHtml descr)
                <> constField "caption-style" captionStyle
                <> constField "title" title
        makeItem ("" :: String) >>= loadAndApplyTemplate templateName ctx
    where templateName = "templates/youtube-embed.html"


compileYoutubeMedia :: Compiler (Item String)
compileYoutubeMedia = do
        ident <- getUnderlying
        pubDate <- T.pack <$> getStringFromMeta "published" ident
        name <- T.pack <$> getStringFromMeta "title" ident
        pandocItem <- getResourceBody >>= readPandoc
        case itemBody pandocItem of
            (Pandoc _ (CodeBlock (_, _, kvals) jsonMeta:descr)) -> do
                ytid <- extractPandocAttr "ytid" kvals
                let emb = Embedded ytid name descr "is-6" (Just pubDate)
                formatYoutubeFromMeta emb jsonMeta
            _ -> fail $ "YouTube resource " <> show ident <> " does not have expected structure"

embedYoutubeVideos :: Block -> Compiler Block
embedYoutubeVideos orig@(CodeBlock attr jsonMeta)
    | not ("youtube" `elem` classes) = return orig
    | otherwise = do
        ytid <- extractPandocAttr "ytid" kvals
        let emb = Embedded ytid "" [] "is-6 has-text-centered" Nothing
        ytEmbedItem <- formatYoutubeFromMeta emb jsonMeta
        let ytContent = RawBlock "html" $ T.pack $ itemBody ytEmbedItem
        return $ Div (elId, classes, []) [ytContent]
    where (elId, classes, kvals) = attr

embedYoutubeVideos orig = return orig


scPlayerUrl :: T.Text
scPlayerUrl = "https://w.soundcloud.com/player/?url="

scApiUrl :: T.Text -> T.Text
scApiUrl trackId = scPlayerUrl <> "https%3A//api.soundcloud.com/tracks/" <> trackId 
                 <> "&amp;color=ff5500&amp;auto_play=false&amp;hide_related=false"
                 <> "&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false"


formatSoundcloudFromMeta :: Embedded -> T.Text -> Compiler (Item String)
formatSoundcloudFromMeta (Embedded trackId name descr captionStyle mbUploadDate) jsonMeta = do
        rawObj <- grabJsonObj jsonMeta
        title <- if T.null name
                 then extractStringOrFail "name" rawObj
                 else return (T.unpack name)
        url <- extractStringOrFail "url" rawObj
        let embedUrl = scApiUrl trackId
        -- only override name/description if actually provided
        let nameIns = if T.null name
                      then id
                      else HMS.insert "name" (Aes.String name)
        let uplDateInsert = case mbUploadDate of
                Nothing -> id
                Just dt -> HMS.insert "uploadDate" (Aes.String dt)
        descrText <- descrTextM descr (T.pack title)
        let newObj = nameIns $ uplDateInsert
                   $ HMS.insert "description" (Aes.String descrText)
                   $ HMS.insert "embedUrl" (Aes.String embedUrl) rawObj
        let ctx = constField "embed-url" (T.unpack embedUrl)
                <> constField "soundcloud-url" url
                <> constField "soundcloud-meta" (jsonString newObj)
                <> constField "caption-style" captionStyle
                <> constField "caption" (descrAsHtml descr)
                <> constField "title" title
        makeItem ("" :: String) >>= loadAndApplyTemplate templateName ctx
    where templateName = "templates/soundcloud-embed.html"


compileSoundcloudMedia :: Compiler (Item String)
compileSoundcloudMedia = do
        ident <- getUnderlying
        pubDate <- T.pack <$> getStringFromMeta "published" ident
        name <- T.pack <$> getStringFromMeta "title" ident
        pandocItem <- getResourceBody >>= readPandoc
        case itemBody pandocItem of
            (Pandoc _ (CodeBlock (_, _, kvals) jsonMeta:descr)) -> do
                trackId <- extractPandocAttr "trackId" kvals
                let emb = Embedded trackId name descr "is-6" (Just pubDate)
                formatSoundcloudFromMeta emb jsonMeta
            _ -> fail $ "SoundCloud resource " <> show ident <> " does not have expected structure"


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


pandocPreviewCompiler :: Compiler (Item String)
pandocPreviewCompiler = getResourceBody >>= readPandoc >>= render
    where render = withItemBody reduceToFirstPara >=> return . writePandoc
          reduceToFirstPara (Pandoc _ blks) = reduceToFirstPara' blks
          reduceToFirstPara' [] = noResult "no preview"
          reduceToFirstPara' (x:xs) = case x of
            Para inl -> return (Pandoc (Meta mempty) [Plain inl])
            _ -> reduceToFirstPara' xs


flagJapaneseText :: Inline -> Inline
flagJapaneseText inl = case inl of
    Span (elId, cls, kvals) inls -> Span (elId, cls, ("lang", "ja"):kvals) inls
    _ -> inl


defaultColWidths :: Block -> Block
defaultColWidths (Table attr caption colSpec thead tbody tfoot)
        = Table attr caption colSpec' thead tbody tfoot
    where colSpec' = [(fst x, ColWidthDefault) | x <- colSpec]

defaultColWidths orig = orig

genericPageCompiler :: Compiler (Item String)
genericPageCompiler = getResourceBody >>= readPandoc >>= processPandoc
    where transformBlk = walkPandocM $ return . shiftAndStyleHeadings 1 
                            >=> return . defaultColWidths
                            >=> embedYoutubeVideos >=> formatInlineMetadata
          transformInl = walkPandocM $ return . flagJapaneseText 
          transform = transformBlk >=> transformInl
          processPandoc = withItemBody transform >=> return . writePandoc
