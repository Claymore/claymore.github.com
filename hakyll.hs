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
import System.FilePath (joinPath, splitDirectories, takeDirectory)
import Data.Ord (comparing)
import Data.List (isInfixOf, sortBy)

import Hakyll hiding (chronological)

articlesPerIndexPage :: Int
articlesPerIndexPage = 6

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy image files.
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "javascripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ wordpressRoute
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> arr (renderDateField "published" "%Y-%m-%dT%H:%M:%S%z" "Date unknown")
            >>> addTeaser
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> wordpressUrlsCompiler

    -- Generate index pages
    match "index*.html" $ route idRoute
    metaCompile $ requireAll_ postsPattern
      >>> arr (chunk articlesPerIndexPage . chronological)
      >>^ makeIndexPages

    -- Read templates
    match "templates/*" $ compile templateCompiler

postsPattern :: Pattern (Page String)
postsPattern = predicate (\i -> matches "posts/*.markdown" i)

wordpressRoute :: Routes
wordpressRoute =
    gsubRoute "posts/" (const "blog/") `composeRoutes`
        gsubRoute "^blog/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (map replaceWithSlash) `composeRoutes`
            gsubRoute ".markdown" (const "/index.html")
    where replaceWithSlash c = if c == '-' || c == '_'
                                   then '/'
                                   else c

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
        maxn = nposts `div` articlesPerIndexPage +
               if (nposts `mod` articlesPerIndexPage /= 0) then 1 else 0
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
        lab = if (d > 0) then "&laquo; Older Posts" else "Newer Posts &raquo;"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "index.html"
                    _ -> "index" ++ (show $ n + d) ++ ".html"

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
                         preEscapedString "Read on &rarr;"

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
