{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Dax
  ( Endpoint
  , API
  , Route
  , ParamDecoder
  , endpoint
  , get
  , post
  , put
  , patch
  , delete
  , static
  , capture
  , query
  , header
  , setHeader
  , constantHeader
  , application
  , sandbox
  , autoParamDecoder
  , documentation
  , NoEffects
  -- Convenience re-exports.
  -- Dax should come with batteries. It won't recommend writing custom JSON
  -- encoders but exports `ToJSON` to support writing generic encoders without
  -- needing to take an explicit dependency on Aeson.
  , Aeson.ToJSON
  -- Similar to above a user shouldn't need to pull in another library to be
  -- able use generics for decoding parameters.
  , Scotty.Parsable
  -- Re-export types from internal module.
  , Dax.Types.ResponseEncoder
  , Dax.Types.BodyDecoder
  ) where

import "base" Data.Bifunctor (first)
import "base" Data.Foldable (traverse_)
import "base" Data.Maybe (listToMaybe)
import "base" Data.Proxy (Proxy(Proxy))
import "text" Data.Text (Text)
import "text" Data.Text.Encoding (decodeUtf8, encodeUtf8)
import "text" Data.Text.Lazy (fromStrict, toStrict)
import "base" Data.Typeable (Typeable, typeRep)
import "this" Dax.Types
import "http-media" Network.HTTP.Media.MediaType (MediaType)

import qualified "aeson" Data.Aeson as Aeson
import qualified "text" Data.Text as Text
import qualified "http-media" Network.HTTP.Media as Media
import qualified "http-media" Network.HTTP.Media.RenderHeader
import qualified "http-types" Network.HTTP.Types.Status as Status
import qualified "wai" Network.Wai as Wai
import qualified "scotty" Web.Scotty as Scotty

data Route m a where
  Get :: Typeable a => [ResponseEncoder a] -> Route m (Result m a)
  Post
    :: (Typeable a, Typeable b)
    => [BodyDecoder a]
    -> [ResponseEncoder b]
    -> Route m (a -> Result m b)
  Put
    :: (Typeable a, Typeable b)
    => [BodyDecoder a]
    -> [ResponseEncoder b]
    -> Route m (a -> Result m b)
  Delete :: Typeable a => [ResponseEncoder a] -> Route m (Result m a)
  Patch
    :: (Typeable a, Typeable b)
    => [BodyDecoder a]
    -> [ResponseEncoder b]
    -> Route m (a -> Result m b)
  PathSegmentStatic :: Text -> Route m b -> Route m b
  PathSegmentCapture
    :: (Typeable a) => ParamDecoder a -> Route m b -> Route m (a -> b)
  QueryParamCapture
    :: (Typeable a)
    => Text
    -> ParamDecoder a
    -> Route m b
    -> Route m (Maybe a -> b)
  Header
    :: (Typeable a)
    => Text
    -> ParamDecoder a
    -> Route m b
    -> Route m (Maybe a -> b)
  SetHeader :: Text -> (b -> Text) -> Route m a -> Route m (b, a)
  ConstantHeader :: Text -> Text -> Route m a -> Route m a

type family Result m a where
  Result NoEffects x = x
  Result m x = m x

data Endpoint m where
  Endpoint :: Route m a -> a -> Endpoint m

type API m = [Endpoint m]

newtype ParamDecoder a = ParamDecoder
  { parse :: Text -> Either Text a
  }

autoParamDecoder :: Scotty.Parsable a => ParamDecoder a
autoParamDecoder =
  ParamDecoder {parse = first toStrict . Scotty.parseParam . fromStrict}

endpoint :: Route m a -> a -> Endpoint m
endpoint = Endpoint

get :: Typeable a => [ResponseEncoder a] -> Route m (Result m a)
get = Get

post ::
     (Typeable a, Typeable b)
  => [BodyDecoder a]
  -> [ResponseEncoder b]
  -> Route m (a -> Result m b)
post = Post

put ::
     (Typeable a, Typeable b)
  => [BodyDecoder a]
  -> [ResponseEncoder b]
  -> Route m (a -> Result m b)
put = Put

delete :: (Typeable a) => [ResponseEncoder a] -> Route m (Result m a)
delete = Delete

patch ::
     (Typeable a, Typeable b)
  => [BodyDecoder a]
  -> [ResponseEncoder b]
  -> Route m (a -> Result m b)
patch = Patch

static :: Text -> Route m a -> Route m a
static = PathSegmentStatic

capture :: Typeable a => ParamDecoder a -> Route m b -> Route m (a -> b)
capture = PathSegmentCapture

query ::
     Typeable a => Text -> ParamDecoder a -> Route m b -> Route m (Maybe a -> b)
query = QueryParamCapture

header ::
     Typeable a => Text -> ParamDecoder a -> Route m b -> Route m (Maybe a -> b)
header = Header

setHeader :: Text -> (b -> Text) -> Route m a -> Route m (b, a)
setHeader = SetHeader

constantHeader :: Text -> Text -> Route m a -> Route m a
constantHeader = ConstantHeader

-- |
-- Interpret the API as a WAI application.
application :: (forall x. Result m x -> IO x) -> API m -> IO Wai.Application
application runM api =
  Scotty.scottyApp $
    -- The default Scotty handler puts the error message in the response body.
    -- Overwrite this with a blank 500 error for security.
   do
    Scotty.defaultHandler (\_ -> finish Status.internalServerError500)
    traverse_ (serveEndpoint runM) api

-- |
-- A simple application that cannot communicate with the outside world.
sandbox :: API NoEffects -> IO Wai.Application
sandbox = application pure

data NoEffects a

serveEndpoint ::
     (forall x. Result m x -> IO x) -> Endpoint m -> Scotty.ScottyM ()
serveEndpoint runM (Endpoint route handler) =
  serveEndpoint' runM End route (pure handler)

serveEndpoint' ::
     (forall x. Result m x -> IO x)
  -> Path
  -> Route m a
  -> Scotty.ActionM a
  -> Scotty.ScottyM ()
serveEndpoint' runM path route f =
  case route of
    Get encoders ->
      Scotty.get (toPath path) $ handleWithoutBody encoders (runM <$> f)
    Post decoders encoders ->
      Scotty.post (toPath path) $ handle decoders encoders ((runM .) <$> f)
    Put decoders encoders -> do
      Scotty.put (toPath path) $ handle decoders encoders ((runM .) <$> f)
    Delete encoders ->
      Scotty.delete (toPath path) $ handleWithoutBody encoders (runM <$> f)
    Patch decoders encoders -> do
      Scotty.patch (toPath path) $ handle decoders encoders ((runM .) <$> f)
    PathSegmentStatic name sub -> serveEndpoint' runM (Static name path) sub f
    PathSegmentCapture decoder sub ->
      serveEndpoint'
        runM
        (Capture name path)
        sub
        (f <*> decodeParam name decoder)
      where name = paramName sub
    QueryParamCapture name decoder sub ->
      serveEndpoint' runM path sub (f <*> decodeQueryParam name decoder)
    Header name decoder sub ->
      serveEndpoint' runM path sub (f <*> decodeHeader name decoder)
    SetHeader name encoder sub ->
      serveEndpoint' runM path sub $ do
        (value, g) <- f
        Scotty.setHeader (fromStrict name) (fromStrict (encoder value))
        pure g
    ConstantHeader name value sub ->
      serveEndpoint' runM path sub $
      Scotty.setHeader (fromStrict name) (fromStrict value) >> f

handle ::
     [BodyDecoder a]
  -> [ResponseEncoder b]
  -> Scotty.ActionM (a -> IO b)
  -> Scotty.ActionM ()
handle decoders encoders f = do
  decoder <- chooseDecoder decoders
  encoder <- chooseEncoder encoders
  content <- decodeBody decoder
  response <- f <*> pure content
  respond encoder response

handleWithoutBody ::
     [ResponseEncoder a] -> Scotty.ActionM (IO a) -> Scotty.ActionM ()
handleWithoutBody encoders f = do
  encoder <- chooseEncoder encoders
  response <- f
  respond encoder response

decodeBody :: BodyDecoder a -> Scotty.ActionM a
decodeBody decoder = do
  body <- Scotty.body
  maybe (finish Status.badRequest400) pure (decode decoder body)

chooseDecoder :: [BodyDecoder a] -> Scotty.ActionM (BodyDecoder a)
chooseDecoder decoders = do
  contentType <- fmap (encodeUtf8 . toStrict) <$> Scotty.header "Content-Type"
  let decoder =
        maybe
          (listToMaybe decoders)
          (Media.mapContentMedia (choice <$> decoders))
          contentType
  maybe (finish Status.unsupportedMediaType415) pure decoder
  where
    choice decoder = (bodyMediaType decoder, decoder)

chooseEncoder :: [ResponseEncoder a] -> Scotty.ActionM (ResponseEncoder a)
chooseEncoder encoders = do
  accept <- fmap (encodeUtf8 . toStrict) <$> Scotty.header "Accept"
  let encoder =
        maybe
          (listToMaybe encoders)
          (Media.mapAcceptMedia (choice <$> encoders))
          accept
  maybe (finish Status.notAcceptable406) pure encoder
  where
    choice encoder = (mediaType encoder, encoder)

paramName :: Route m b -> Text
paramName sub = Text.pack (show (routeDepth 0 sub))

routeDepth :: Int -> Route m b -> Int
routeDepth n route =
  case route of
    Get _ -> n
    Post _ _ -> n
    Put _ _ -> n
    Delete _ -> n
    Patch _ _ -> n
    PathSegmentStatic _ sub -> routeDepth (n + 1) sub
    PathSegmentCapture _ sub -> routeDepth (n + 1) sub
    QueryParamCapture _ _ sub -> routeDepth n sub
    Header _ _ sub -> routeDepth n sub
    SetHeader _ _ sub -> routeDepth n sub
    ConstantHeader _ _ sub -> routeDepth n sub

decodeParam :: Text -> ParamDecoder a -> Scotty.ActionM a
decodeParam name ParamDecoder {parse} = do
  value <- Scotty.param (fromStrict name)
  case parse value of
    Right x -> pure x
    Left _ -> Scotty.next

decodeQueryParam :: Text -> ParamDecoder a -> Scotty.ActionM (Maybe a)
decodeQueryParam name ParamDecoder {parse} = do
  params <- Scotty.params
  let encoded = lookup (fromStrict name) params
  let value = traverse (parse . toStrict) encoded
  either (const (finish Status.badRequest400)) pure value

decodeHeader :: Text -> ParamDecoder a -> Scotty.ActionM (Maybe a)
decodeHeader name ParamDecoder {parse} = do
  encoded <- Scotty.header (fromStrict name)
  let value = traverse (parse . toStrict) encoded
  either (const (finish Status.badRequest400)) pure value

respond :: ResponseEncoder a -> IO a -> Scotty.ActionM ()
respond ResponseEncoder {encode, mediaType} x = do
  Scotty.setHeader "Content-Type" (fromStrict $ renderMediaType mediaType)
  Response {body, status} <- encode <$> Scotty.liftAndCatchIO x
  Scotty.status status
  Scotty.raw body

finish :: Status.Status -> Scotty.ActionM a
finish status = do
  Scotty.status status
  Scotty.finish

renderMediaType :: MediaType -> Text
renderMediaType = decodeUtf8 . Network.HTTP.Media.RenderHeader.renderHeader

data Path
  = End
  | Static Text
           Path
  | Capture Text
            Path

toPath :: Path -> Scotty.RoutePattern
toPath = Scotty.capture . Text.unpack . toPath' ""

toPath' :: Text -> Path -> Text
toPath' acc End = acc
toPath' acc (Static segment rest) = toPath' ("/" <> segment <> acc) rest
toPath' acc (Capture name rest) = toPath' ("/:" <> name <> acc) rest

-- |
-- Interpret an API as documentation
documentation :: API m -> [Doc]
documentation = fmap docForEndpoint

docForEndpoint :: Endpoint m -> Doc
docForEndpoint (Endpoint route _) = docForRoute route (Doc "" "" "" [] [] [])

docForRoute :: Route m a -> Doc -> Doc
docForRoute (Get encoders) doc =
  doc {method = "GET", responseTypes = typeOfEncoders encoders}
docForRoute (Post _ encoders) doc =
  doc {method = "POST", responseTypes = typeOfEncoders encoders}
docForRoute (Put _ encoders) doc =
  doc {method = "PUT", responseTypes = typeOfEncoders encoders}
docForRoute (Delete encoders) doc =
  doc {method = "DELETE", responseTypes = typeOfEncoders encoders}
docForRoute (Patch _ encoders) doc =
  doc {method = "PATCH", responseTypes = typeOfEncoders encoders}
docForRoute (PathSegmentStatic name sub) doc =
  docForRoute sub $ doc {path = path doc <> "/" <> name}
docForRoute (PathSegmentCapture (_ :: ParamDecoder a) sub) doc =
  docForRoute sub $
  doc {path = path doc <> "/:<" <> typeName (Proxy :: Proxy a) <> ">"}
docForRoute (QueryParamCapture name decoder sub) doc =
  docForRoute sub doc {queryParams = (name, typeName decoder) : queryParams doc}
docForRoute (Header name decoder sub) doc =
  docForRoute
    sub
    doc {requestHeaders = (name, typeName decoder) : requestHeaders doc}
docForRoute (SetHeader name encoder sub) doc =
  docForRoute
    sub
    doc
      { responseHeaders =
          (name, "<" <> typeName encoder <> ">") : responseHeaders doc
      }
docForRoute (ConstantHeader name value sub) doc =
  docForRoute sub doc {responseHeaders = (name, value) : responseHeaders doc}

typeOfEncoders ::
     forall a. Typeable a
  => [ResponseEncoder a]
  -> Text
typeOfEncoders _ = Text.pack . show . typeRep $ (Proxy :: Proxy a)

typeName :: Typeable a => m a -> Text
typeName = Text.pack . show . typeRep

data Doc = Doc
  { path :: Text
  , method :: Text
  , responseTypes :: Text
  , queryParams :: [(Text, Text)]
  , requestHeaders :: [(Text, Text)]
  , responseHeaders :: [(Text, Text)]
  } deriving (Show)
