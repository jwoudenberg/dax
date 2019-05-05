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
import "base" Data.Foldable (for_, traverse_)
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
import qualified "text" Data.Text.Lazy as Text.Lazy
import qualified "http-media" Network.HTTP.Media as Media
import qualified "http-media" Network.HTTP.Media.RenderHeader
import qualified "http-types" Network.HTTP.Types.Method as Method
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

-- |
-- Interpret the API as a WAI application.
application :: (forall x. Result m x -> IO x) -> API m -> IO Wai.Application
application runM = Scotty.scottyApp . traverse_ (serveEndpoint runM)

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
    Get encoders -> handleWithoutBody Method.GET path encoders (runM <$> f)
    Post decoders encoders ->
      handle Method.POST path decoders encoders ((runM .) <$> f)
    Put decoders encoders -> do
      handle Method.PUT path decoders encoders ((runM .) <$> f)
    Delete encoders ->
      handleWithoutBody Method.DELETE path encoders (runM <$> f)
    Patch decoders encoders -> do
      handle Method.PATCH path decoders encoders ((runM .) <$> f)
    PathSegmentStatic name sub -> serveEndpoint' runM (Static name path) sub f
    PathSegmentCapture decoder sub ->
      serveEndpoint'
        runM
        (Capture name path)
        sub
        (f <*> decodeParam name decoder)
      where name = paramName sub
    QueryParamCapture name decoder sub ->
      serveEndpoint' runM path sub (f <*> decodeOptionalParam name decoder)

handle ::
     Method.StdMethod
  -> Path
  -> [BodyDecoder a]
  -> [ResponseEncoder b]
  -> Scotty.ActionM (a -> IO b)
  -> Scotty.ScottyM ()
handle method path decoders encoders f =
  Scotty.addroute method (toPath path) $ do
    body <- Scotty.body
    decoder' <- chooseDecoder decoders
    encoder' <- chooseEncoder encoders
    either Scotty.status id $ do
      decoder <- toEither Status.unsupportedMediaType415 decoder'
      encoder <- toEither Status.notAcceptable406 encoder'
      content <- toEither Status.badRequest400 (decode decoder body)
      pure $ respond encoder =<< f <*> pure content

handleWithoutBody ::
     Method.StdMethod
  -> Path
  -> [ResponseEncoder a]
  -> Scotty.ActionM (IO a)
  -> Scotty.ScottyM ()
handleWithoutBody method path encoders f =
  Scotty.addroute method (toPath path) $ do
    encoder' <- chooseEncoder encoders
    either Scotty.status id $ do
      encoder <- toEither Status.notAcceptable406 encoder'
      pure $ respond encoder =<< f

toEither :: e -> Maybe a -> Either e a
toEither e = maybe (Left e) Right

chooseDecoder :: [BodyDecoder a] -> Scotty.ActionM (Maybe (BodyDecoder a))
chooseDecoder decoders = do
  contentType <- fmap (encodeUtf8 . toStrict) <$> Scotty.header "Content-Type"
  pure $
    maybe
      (listToMaybe decoders)
      (Media.mapContentMedia (choice <$> decoders))
      contentType
  where
    choice decoder = (bodyMediaType decoder, decoder)

chooseEncoder ::
     [ResponseEncoder a] -> Scotty.ActionM (Maybe (ResponseEncoder a))
chooseEncoder encoders = do
  accept <- fmap (encodeUtf8 . toStrict) <$> Scotty.header "Accept"
  pure $
    maybe
      (listToMaybe encoders)
      (Media.mapAcceptMedia (choice <$> encoders))
      accept
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

decodeParam :: Text -> ParamDecoder a -> Scotty.ActionM a
decodeParam name ParamDecoder {parse} = do
  value <- Scotty.param (fromStrict name)
  case parse value of
    Right x -> pure x
    Left _ -> Scotty.next

decodeOptionalParam :: Text -> ParamDecoder a -> Scotty.ActionM (Maybe a)
decodeOptionalParam name ParamDecoder {parse} = do
  params <- Scotty.params
  let encoded = lookup (fromStrict name) params
  let value = traverse (parse . toStrict) encoded
  either (const Scotty.next) pure value

respond :: ResponseEncoder a -> IO a -> Scotty.ActionM ()
respond ResponseEncoder {encode, mediaType} x = do
  Scotty.setHeader "Content-Type" (fromStrict $ renderMediaType mediaType)
  Response {body, status, headers} <- encode <$> Scotty.liftAndCatchIO x
  Scotty.status status
  for_ headers $ \(name, value) ->
    Scotty.setHeader
      (Text.Lazy.pack $ show name)
      (fromStrict $ decodeUtf8 value)
  Scotty.raw body

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
docForEndpoint (Endpoint route _) = docForRoute route (Doc "" "" "" [])

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
docForRoute (QueryParamCapture name _ sub) doc =
  docForRoute sub doc {queryParams = name : queryParams doc}

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
  , queryParams :: [Text]
  } deriving (Show)
