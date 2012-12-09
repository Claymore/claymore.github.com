{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), arr, (&&&), (>>^), (***), (|||))
import Data.Monoid (mempty, mconcat, mappend)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Internal (preEscapedString)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as B
import System.FilePath (joinPath, splitDirectories, takeDirectory, dropFileName, takeBaseName)
import Data.Ord (comparing)
import Data.List (isInfixOf, sortBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Map (findWithDefault)
import Data.Char (toLower)
import qualified Data.Set as S
import qualified Text.HTML.TagSoup as TS

import Hakyll hiding (chronological, withUrls)

type SPage = Page String

data BlogConfiguration = BlogConfiguration
    { paginate :: Int
    , authorName :: String
    , authorEmail :: String
    , blogTitle :: String
    , blogSubtitle :: String
    , rootUrl :: String
    , excerptLink :: String
    , dateFormat :: String
    , githubUser :: String
    , googlePlusId :: String
    , googlePlusHidden :: Bool
    , disqusShortName :: String
    , defaultAsides :: [String]
    , recentPosts :: Int
    , googleAnalyticsId :: String
    , facebookLike :: Bool
    , twitterButton :: Bool
    , twitterUser :: String
    , tweetCount :: String
    , twitterShowReplies :: String
    , pinboardUser :: String
    , pinboardCount :: String
    , deliciousUser :: String
    , deliciousCount :: String
    , googlePlusOne :: Bool
    , googlePlusOneSize :: String
    } deriving (Show, Eq)

blogConfiguration = BlogConfiguration
    { paginate = 2
    , authorName = "Alexey Bobyakov"
    , authorEmail = "claymore.ws@gmail.com"
    , blogTitle = "Shirohida"
    , blogSubtitle = "Programming tips, how-to, thoughts and rants"
    , rootUrl = "http://claymore.github.com"
    , excerptLink = "Read on &rarr;"
    , dateFormat = "%B %e, %Y"
    , githubUser = "Claymore"
    , googlePlusId = "102481707573568225620"
    , googlePlusHidden = True
    , disqusShortName = "shirohida"
    , defaultAsides = ["templates/includes/custom/asides/about.html", "templates/includes/asides/recent_posts.html", "templates/includes/asides/github.html", "templates/includes/asides/googleplus.html"]
    , recentPosts = 5
    , googleAnalyticsId = "UA-2129201-2"
    , facebookLike = False
    , twitterButton = True
    , twitterUser = ""
    , tweetCount = ""
    , twitterShowReplies = ""
    , googlePlusOne = False
    , googlePlusOneSize = "medium"
    , pinboardUser = ""
    , pinboardCount = ""
    , deliciousUser = ""
    , deliciousCount = ""
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = (blogTitle blogConfiguration)
    , feedDescription = (blogSubtitle blogConfiguration)
    , feedAuthorName = (authorName blogConfiguration)
    , feedAuthorEmail = (authorEmail blogConfiguration)
    , feedRoot = (rootUrl blogConfiguration)
    }

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "./deploy.sh"
    }

main :: IO ()
main = hakyllWith config $ do
    -- Compress CSS
    match "stylesheets/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy image files.
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "javascripts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.png" $ do
        route   idRoute
        compile copyFileCompiler

    -- Tags
    create "categories" $
        requireAll ("posts/*" `mappend` inGroup Nothing) (\_ ps -> readCategories ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "categories/*" $ route categoryRoute
    metaCompile $ require_ "categories"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Render RSS feed
    match "atom.xml" $ route idRoute
    create "atom.xml" $ requireAll_ ("posts/*" `mappend` inGroup Nothing)
        >>> arr (reverse . chronological)
        >>> renderAtom feedConfiguration

    match "templates/includes/asides/recent_posts.html" $ do
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> setFieldPageList (take (recentPosts blogConfiguration) . chronological) "templates/includes/recent_post.html" "recentposts" ("posts/*" `mappend` inGroup (Just "raw"))
            >>> arr applySelf

    match "templates/includes/asides/*" $ do
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> setBlogFields
            >>> arr applySelf

    match "templates/includes/custom/asides/*" $ do
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> setBlogFields
            >>> arr applySelf

    match "archives.html" $ do
        route   $ customRoute (\f -> "blog/" ++ takeBaseName(show(f)) ++ "/index.html")
        create "archives.html" $ constA mempty
            >>> addDefaultFields
            >>> arr (setField "title" "Blog Archives")
            >>> addDefaultTemplateFields
            >>> setFieldPageList chronological "templates/includes/archive_post.html" "posts" ("posts/*" `mappend` inGroup Nothing)
            >>> applyTemplateCompiler "templates/layouts/archive.html"
            >>> applyTemplateCompiler "templates/layouts/default.html"
            >>> wordpressUrlsCompiler

    -- Sitemap
    match  "sitemap.xml" $ route idRoute
    create "sitemap.xml" $ constA mempty
        >>> setFieldPageList chronological "templates/postsitemap.xml" "posts" ("posts/*" `mappend` inGroup Nothing)
        >>> applyTemplateCompiler "templates/sitemap.xml"

    group "raw" $ do
        match "posts/*" $ do
            compile $ readPageCompiler
                >>> addDefaultFields
                >>> arr (renderField "path" "posturl" pathToUrl)

    -- Render posts
    match "posts/*" $ do
        route   $ postsRoute
        compile $ pageCompiler
            >>> arr (renderDateField "date" (dateFormat blogConfiguration) "Date unknown")
            >>> arr (renderDateField "published" "%Y-%m-%dT%H:%M:%S%z" "Date unknown")
            >>> arr (renderDateField "shortdate" "%b %e" "Date unknown")
            >>> arr (renderDateField "year" "%Y" "Date unknown")
            >>> renderModificationTime "lastmod" "%Y-%m-%dT%H:%M:%S%z"
            >>> arr (copyBodyToField "description")
            >>> addDefaultTemplateFields
            >>> renderTagsFieldWith getCategories "prettycats" (fromCapture "categories/*")
            >>> requireAllA ("posts/*" `mappend` inGroup (Just "raw")) addNearbyPosts
            >>> addTeaser
            >>> applyTemplateCompiler "templates/layouts/post.html"
            >>> applyTemplateCompiler "templates/layouts/default.html"
            >>> wordpressUrlsCompiler

    -- Generate index pages
    match "index.html" $ route idRoute
    match "index*.html" $ route indexRoute
    metaCompile $ requireAll_ ("posts/*" `mappend` inGroup Nothing)
      >>> arr (chunk (paginate blogConfiguration) . chronological)
      >>^ makeIndexPages

    -- Read templates
    match "templates/*" $ compile templateCompiler
    match "templates/layouts/*" $ compile templateCompiler
    match "templates/includes/*" $ compile templateCompiler
    match "templates/includes/post/*" $ compile templateCompiler

indexRoute :: Routes
indexRoute = customRoute (\f -> "blog/page/" ++ (reverse . (drop 5) . reverse . (drop 5 . show) $ f) ++ "/index.html")

categoryRoute :: Routes
categoryRoute = customRoute (\f -> "blog/" ++ (map (toLower . replace '.' '-') $ show f) ++ "/index.html")
    where replace s r c
           | c == s = r
           | otherwise = c

pathToUrl :: String -> String
pathToUrl path = "/blog/" ++ year ++ "/" ++ month ++ "/" ++ day ++ "/" ++ title ++ "/"
    where name = takeBaseName path
          date = take 11 name
          year = take 4 date
          month = take 2 . drop 5 $ date
          day = take 2 . drop 8 $ date
          title = drop 11 name

postsRoute :: Routes
postsRoute = customRoute (\f -> (drop 1 . pathToUrl . show $ f) ++ "index.html")

setBlogFields :: Compiler (Page String) (Page String)
setBlogFields =
    arr (setField "author" (authorName blogConfiguration))
    >>> arr (setField "blogtitle" (blogTitle blogConfiguration))
    >>> arr (setField "blogsubtitle" (blogSubtitle blogConfiguration))
    >>> arr (setField "githubuser" (githubUser blogConfiguration))
    >>> arr (setField "googleplusid" (googlePlusId blogConfiguration))
    >>> arr (setField "disqusshortname" (disqusShortName blogConfiguration))
    >>> arr (setField "host" (rootUrl blogConfiguration))
    >>> arr (setField "googleanalyticsid" (googleAnalyticsId blogConfiguration))
    >>> arr (setField "googleplusonesize" (googlePlusOneSize blogConfiguration))
    >>> arr (setField "twitteruser" (twitterUser blogConfiguration))
    >>> arr (setField "tweetcount" (tweetCount blogConfiguration))
    >>> arr (setField "twittershowreplies" (twitterShowReplies blogConfiguration))
    >>> arr (setField "pinboarduser" (pinboardUser blogConfiguration))
    >>> arr (setField "pinboardcount" (pinboardCount blogConfiguration))
    >>> arr (setField "delicioususer" (deliciousUser blogConfiguration))
    >>> arr (setField "deliciouscount" (deliciousCount blogConfiguration))

getCategories = map trim . splitAll "," . getField "categories"
readCategories = readTagsWith getCategories

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "categories/*"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
    >>> arr (map stripIndexLink)
    >>> pageListCompiler recentFirst "templates/includes/archive_post.html"
    >>> arr (copyBodyToField "posts" . fromBody)
    >>> arr (setField "title" ("Category: " ++ tag))
    >>> addDefaultTemplateFields
    >>> applyTemplateCompiler "templates/layouts/category.html"
    >>> applyTemplateCompiler "templates/layouts/default.html"

-- | Compiler form of 'wordpressUrls' which automatically turns index.html
-- links into just the directory name
--
wordpressUrlsCompiler :: Compiler (Page String) (Page String)
wordpressUrlsCompiler = getRoute &&& id >>^ uncurry convert
  where
    convert Nothing  = id
    convert (Just r) = fmap (wordpressUrls r)

-- | Convert URLs to WordPress style in HTML
--
wordpressUrls :: String  -- ^ Path to the site root
              -> String  -- ^ HTML to convert
              -> String  -- ^ Resulting HTML
wordpressUrls root = withUrls convert
  where
    convert x = replaceAll "/index.html" (const "/") x

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@.
addPostList :: String -> Compiler (Page String, [Page String]) (Page String)
addPostList tmp = setFieldA "posts" $
    arr chronological
        >>> require (parseIdentifier tmp) (\p t -> map (applyTemplate t) p)
        >>> arr mconcat >>> arr pageBody

-- | Helper function for index page metacompilation: generate
-- appropriate number of index pages with correct names and the
-- appropriate posts on each one.
makeIndexPages :: [[Page String]] ->
                  [(Identifier (Page String), Compiler () (Page String))]
makeIndexPages ps = map doOne (zip [1..] ps)
  where doOne (n, ps) = (indexIdentifier n, makeIndexPage n maxn ps)
        maxn = nposts `div` (paginate blogConfiguration) +
               if (nposts `mod` (paginate blogConfiguration) /= 0) then 1 else 0
        nposts = sum $ map length ps
        indexIdentifier n = parseIdentifier url
          where url = "index" ++ (if (n == 1) then "" else show n) ++ ".html"

addDefaultTemplateFields = addDefaultFields
    >>> setBlogFields
    >>> setFieldPageList sortAsidesByIndex "templates/includes/aside.html" "asides" asidesList
    >>> isGooglePlusHidden
    >>> (arr (setField "googleplushidden" "") ||| arr (setField "googleplushidden" "googleplus-hidden"))
    >>> isGoogleAnalyticsEnabled
    >>> (arr (setField "googleanalytics" "") |||
        addFieldTemplate "googleanalytics" "templates/includes/google_analytics.html")
    >>> isTwitterButtonEnabled
    >>> ((arr (setField "twittersharing" "") >>> arr (setField "posttwittersharing" "")) |||
        (addFieldTemplate "twittersharing" "templates/includes/twitter_sharing.html"
        >>> addFieldTemplate "posttwittersharing" "templates/includes/post/twitter_sharing.html"))
    >>> isFacebookLikeEnabled
    >>> ((arr (setField "facebooklike" "") >>> arr (setField "postfacebooklike" "")) |||
        (addFieldTemplate "facebooklike" "templates/includes/facebook_like.html"
        >>> addFieldTemplate "postfacebooklike" "templates/includes/post/facebook_like.html"))
    >>> isGooglePlusOneEnabled
    >>> ((arr (setField "googleplusone" "") >>> arr (setField "postgoogleplusone" "")) |||
        (addFieldTemplate "googleplusone" "templates/includes/google_plus_one.html"
        >>> addFieldTemplate "postgoogleplusone" "templates/includes/post/google_plus_one.html"))
    >>> isDisqusEnabled
    >>> (arr (setField "disqus" "") |||
        addFieldTemplate "disqus" "templates/includes/disqus.html")
    >>> addFieldTemplate "head" "templates/includes/head.html"
    >>> addFieldTemplate "header" "templates/includes/header.html"
    >>> addFieldTemplate "navigation" "templates/includes/navigation.html"
    >>> addFieldTemplate "footer" "templates/includes/footer.html"
    >>> addFieldTemplate "afterfooter" "templates/includes/after_footer.html"

addFieldTemplate :: String -> Identifier Template -> Compiler (Page String) (Page String)
addFieldTemplate key template = (id &&& id) >>> setFieldA key (applyTemplateCompiler template >>> arr pageBody)

-- Make a single index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
makeIndexPage :: Int -> Int -> [Page String] -> Compiler () (Page String)
makeIndexPage n maxn posts =
    constA (mempty, posts)
    >>> addPostList "templates/includes/index_post.html"
    >>> arr (setField "navlinkolder" (indexNavLink n 1 maxn))
    >>> arr (setField "navlinknewer" (indexNavLink n (-1) maxn))
    >>> arr (setField "title" "Main")
    >>> addDefaultTemplateFields
    >>> applyTemplateCompiler "templates/layouts/post.html"
    >>> applyTemplateCompiler "templates/layouts/index.html"
    >>> applyTemplateCompiler "templates/layouts/default.html"
    >>> wordpressUrlsCompiler

isGoogleAnalyticsEnabled = arr (\p -> if enabled p then Right p else Left p)
    where enabled = (/= "") . getField "googleanalyticsid"

isDisqusEnabled = arr (\p -> if enabled p then Right p else Left p)
    where enabled = (/= "") . getField "disqusshortname"

isGooglePlusHidden = arr (\p -> if h then Right p else Left p)
    where h = googlePlusHidden blogConfiguration

isTwitterButtonEnabled = arr (\p -> if enabled then Right p else Left p)
    where enabled = twitterButton blogConfiguration

isFacebookLikeEnabled = arr (\p -> if enabled then Right p else Left p)
    where enabled = facebookLike blogConfiguration

isGooglePlusOneEnabled = arr (\p -> if enabled then Right p else Left p)
    where enabled = googlePlusOne blogConfiguration

-- Generate navigation link HTML for stepping between index pages.
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $
                   (preEscapedString lab)
        lab = if (d > 0) then "&laquo; Older" else "Newer &raquo;"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "index.html"
                    _ -> "blog/page/" ++ (show $ n + d) ++ "/index.html"

-- | Turns body of the page into the teaser: anything up to the
-- <!--MORE--> mark is the teaser, except for text between the
-- <!--NOTEASERBEGIN--> and <!--NOTEASEREND--> marks (useful for
-- keeping images out of teasers).
--
addTeaser :: Compiler (Page String) (Page String)
addTeaser = arr (copyBodyToField "teaser")
    >>> arr (changeField "teaser" extractTeaser)
    >>> (arr $ getField "url" &&& id)
    >>> fixTeaserResourceUrls
    >>> (id &&& arr pageBody)
    >>> arr (\(p, b) -> setField "readmore"
                        (if (isInfixOf "<!-- more -->" (pageBody p))
                         then (readMoreLink p) else "") p)
      where
        extractTeaser = unlines . (noTeaser . extractTeaser') . lines
        extractTeaser' = takeWhile (/= "<!-- more -->")

        noTeaser [] = []
        noTeaser ("<!--NOTEASERBEGIN-->" : xs) =
            drop 1 $ dropWhile (/= "<!--NOTEASEREND-->") xs
        noTeaser (x : xs) = x : (noTeaser xs)

        readMoreLink :: Page String -> String
        readMoreLink p = renderHtml $ H.footer $
                         H.a ! A.rel "full-article" ! A.href (toValue $ getField "url" p) $
                         preEscapedString (excerptLink blogConfiguration)

        fixTeaserResourceUrls :: Compiler (String, (Page String)) (Page String)
        fixTeaserResourceUrls = arr $ (\(url, p) -> fixResourceUrls' url p)
          where fixResourceUrls' url p =
                  changeField "teaser" (fixResourceUrls'' (takeDirectory url)) p

fixResourceUrls'' :: String -> String -> String
fixResourceUrls'' path = withUrls (\x -> if '/' `elem` x then x else path ++ "/" ++ x)

-- | Sort pages chronologically. This function assumes that the pages have a
-- @year/month/day/title[.extension]@ naming scheme.
--
chronological :: [Page String] -> [Page String]
chronological = reverse . (sortBy $ comparing pageSortKey)

-- | Generate a sort key for ordering entries on the index page.
--
pageSortKey :: Page String -> String
pageSortKey pg =  datePart
  where path = getField "path" pg
        datePart = joinPath $ take 3 $ drop 1 $ splitDirectories path

-- Split list into equal sized sublists.
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
    where (ys,zs) = splitAt n xs

stripIndexLink :: Page a -> Page a
stripIndexLink = changeField "url" dropFileName

asidesList = list . (map parseIdentifier) . defaultAsides $ blogConfiguration

sortAsidesByIndex = sortBy $ comparing $ fromJust . (flip elemIndex $ (defaultAsides blogConfiguration)) . getField "path"

get0 :: [a] -> [a]
get0 (a:_) = [a]
get0 _     = []

get1 :: [a] -> [a]
get1 (_:a:_) = [a]
get1 _       = []

getPageField key page = findWithDefault [] key (toMap page)

equalPath a b = (getPageField "path" a) == (getPageField "path" b)

-- | Given a post and a list of all posts, return the
--   preceeding and following posts, if they exist.
--
findNeighbours :: Compiler (SPage, [SPage]) (SPage, ([SPage], [SPage]))
findNeighbours =
    arr $ \(cpage, plist) ->
        let slist = (reverse . chronological) plist
            (earlier, later) = break (equalPath cpage) slist
        in (cpage, (get0 (reverse earlier), get1 later))

-- | Add in the previous and next links for a post
--
addNearbyPosts :: Compiler (SPage, [SPage]) SPage
addNearbyPosts =
    arr (id *** recentFirst)
    >>> findNeighbours
    >>> setFieldA "neighbours"
      ((mapCompiler (applyTemplateCompiler "templates/includes/post_previous.html")
        ***
        mapCompiler (applyTemplateCompiler "templates/includes/post_next.html"))
       >>> arr (uncurry (++)) >>> arr mconcat >>> arr pageBody)

-- | Apply a function to each URL on a webpage
--
withUrls :: (String -> String) -> String -> String
withUrls f = renderTags' . map tag . TS.parseTags
  where
    tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
    tag x                = x
    attr (k, v)          = (k, if k `S.member` refs then f v else v)
    refs                 = S.fromList ["src", "href", "data-url", "data-counturl"]
    renderTags' = TS.renderTagsOptions TS.renderOptions
        { TS.optRawTag = (`elem` ["script", "style"]) . map toLower
        }
