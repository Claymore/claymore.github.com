{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), arr, (&&&), (>>^), (***))
import Data.Monoid (mempty, mconcat)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Internal (preEscapedString)
import Text.Blaze ((!), toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as B
import System.FilePath (joinPath, splitDirectories, takeDirectory, dropFileName, takeBaseName)
import Data.Ord (comparing)
import Data.List (isInfixOf, sortBy)

import Hakyll hiding (chronological)

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
    , disqusShortName :: String
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
    , disqusShortName = "shirohida"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = (blogTitle blogConfiguration)
    , feedDescription = (blogSubtitle blogConfiguration)
    , feedAuthorName = (authorName blogConfiguration)
    , feedAuthorEmail = (authorEmail blogConfiguration)
    , feedRoot = (rootUrl blogConfiguration)
    }

main :: IO ()
main = hakyll $ do
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
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "categories/*" $ route $ customRoute (\f -> "blog/" ++ show(f) ++ "/index.html")
    metaCompile $ require_ "categories"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Render RSS feed
    match "atom.xml" $ route idRoute
    create "atom.xml" $ requireAll_ "posts/*"
        >>> arr (reverse . chronological)
        >>> renderAtom feedConfiguration

    match "archives.html" $ do
        route   $ customRoute (\f -> "blog/" ++ takeBaseName(show(f)) ++ "/index.html")
        create "archives.html" $ constA mempty
            >>> arr (setField "title" "Blog Archives")
            >>> setBlogFields
            >>> requireAllA "posts/*" (id *** arr (reverse . chronological) >>> (addPostList "templates/archiveitem.html"))
            >>> applyTemplateCompiler "templates/archive.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> wordpressUrlsCompiler

    -- Sitemap
    match  "sitemap.xml" $ route idRoute
    create "sitemap.xml" $ constA mempty
        >>> requireAllA "posts/*" postListSitemap
        >>> applyTemplateCompiler "templates/sitemap.xml"

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
            >>> setBlogFields
            >>> renderTagsField "prettytags" (fromCapture "categories/*")
            >>> addTeaser
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> wordpressUrlsCompiler

    -- Generate index pages
    match "index.html" $ route idRoute
    match "index*.html" $ route indexRoute
    metaCompile $ requireAll_ "posts/*"
      >>> arr (chunk (paginate blogConfiguration) . chronological)
      >>^ makeIndexPages

    -- Read templates
    match "templates/**" $ compile templateCompiler

indexRoute :: Routes
indexRoute = customRoute (\f -> "blog/page/" ++ (reverse . (drop 5) . reverse . (drop 5 . show) $ f) ++ "/index.html")

postsRoute :: Routes
postsRoute =
    gsubRoute "posts/" (const "blog/") `composeRoutes`
        gsubRoute "^blog/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash) `composeRoutes`
            gsubRoute ".markdown" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c

setBlogFields :: Compiler (Page String) (Page String)
setBlogFields =
    arr (setField "author" (authorName blogConfiguration))
    >>> arr (setField "blogtitle" (blogTitle blogConfiguration))
    >>> arr (setField "blogsubtitle" (blogSubtitle blogConfiguration))
    >>> arr (setField "githubuser" (githubUser blogConfiguration))
    >>> arr (setField "googleplusid" (googlePlusId blogConfiguration))
    >>> arr (setField "disqusshortname" (disqusShortName blogConfiguration))
    >>> arr (setField "host" (rootUrl blogConfiguration))

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "categories/*"

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
    >>> arr (map stripIndexLink)
    >>> pageListCompiler recentFirst "templates/archiveitem.html"
    >>> arr (copyBodyToField "posts" . fromBody)
    >>> arr (setField "title" ("Category: " ++ tag))
    >>> setBlogFields
    >>> applyTemplateCompiler "templates/tag.html"
    >>> applyTemplateCompiler "templates/default.html"

postListSitemap :: Compiler (Page String, [Page String]) (Page String)
postListSitemap = buildList "posts" "templates/postsitemap.xml"

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

-- Make a single index page: inserts posts, sets up navigation links
-- to older and newer article index pages, applies templates.
makeIndexPage :: Int -> Int -> [Page String] -> Compiler () (Page String)
makeIndexPage n maxn posts =
    constA (mempty, posts)
    >>> addPostList "templates/postitem.html"
    >>> arr (setField "navlinkolder" (indexNavLink n 1 maxn))
    >>> arr (setField "navlinknewer" (indexNavLink n (-1) maxn))
    >>> arr (setField "title" "Main")
    >>> setBlogFields
    >>> applyTemplateCompiler "templates/post.html"
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> wordpressUrlsCompiler

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

buildList :: String -> Identifier Template -> Compiler (Page String, [Page String]) (Page String)
buildList field template = setFieldA field $
    arr (reverse . chronological)
        >>> arr (map stripIndexLink)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
