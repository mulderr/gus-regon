-- | Partial implementation of multipart/related
--
-- See: https://tools.ietf.org/html/rfc7230
module Web.BIR.BIR11.Parse where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isPrint)
import Data.Map (Map)
import Text.XML (parseLBS, def, Document)

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map


-- https://tools.ietf.org/html/rfc7230#section-3.2.6
token :: A.Parser B.ByteString
token =
  A.takeWhile1 isTchar
  where
    isTchar c
      =  isAsciiLower c
      || isAsciiUpper c
      || isDigit c
      || c `elem` ("!#$%&'*+-.^_`|~" :: String)

-- https://tools.ietf.org/html/rfc7230#section-3.2.3
ows :: A.Parser ()
ows = do
  _ <- A.takeWhile (\c -> c == ' ' || c == '\t')
  pure ()

-- https://tools.ietf.org/html/rfc7230#section-3.2.6
quotedString :: A.Parser B.ByteString
quotedString = do
  _ <- A.char '"'
  xs <- A.many' (qdtext <|> quotedPair)
  _ <- A.char '"'
  pure $ B.concat xs

-- https://tools.ietf.org/html/rfc7230#section-3.2.6
qdtext :: A.Parser B.ByteString
qdtext =
  A.takeWhile1 isQdtext
  where
    isQdtext c
      =  c == '\t' || c == ' ' || c == '\x21'
      || (c >= '\x23' && c <= '\x5b')
      || (c >= '\x5d' && c <= '\x7e')
      || isObsText c

-- https://tools.ietf.org/html/rfc7230#section-3.2.6
isObsText :: Char -> Bool
isObsText c = c >= '\x80' && c <= '\xff'

-- https://tools.ietf.org/html/rfc7230#section-3.2.6
quotedPair :: A.Parser B.ByteString
quotedPair = do
  xs <- A.many1 $ do
    _ <- A.char '\\'
    c <- A.satisfy (\c -> c == '\t' || c == ' ' || isPrint c || isObsText c)
    pure $ B8.snoc "\\" c
  pure $ B.concat xs

-- Returns type/subtype and a map of parameters
contentTypeParser :: A.Parser (B.ByteString, Map B.ByteString B.ByteString)
contentTypeParser = do
  ty <- token
  _ <- A.char '/'
  subty <- token
  xs <- A.many' $ (ows >> A.char ';' >> ows) *> parameter
  A.endOfInput
  pure (ty <> "/" <> subty, Map.fromList xs)
  where
    parameter = do
      nm <- token
      _ <- A.char '='
      val <- token <|> quotedString
      pure (nm, val)

parseContentType :: B.ByteString -> Either String (B.ByteString, Map B.ByteString B.ByteString)
parseContentType =
  A.parseOnly contentTypeParser

mrelBody :: B.ByteString -> A.Parser [(Map B.ByteString B.ByteString, B.ByteString)]
mrelBody boundary = do
  let bd = "--" <> boundary
      bdnl = bd <> "\r\n"
  _ <- A.takeWhile (/= '-')
  _ <- A.string bdnl
  ps <- A.sepBy1 (mrelPart bd) (A.string "\r\n")
  _ <- A.string "--"
  _ <- A.endOfLine
  A.endOfInput
  pure ps

mrelPart :: B.ByteString -> A.Parser (Map B.ByteString B.ByteString, B.ByteString)
mrelPart bd = do
  hs <- A.many1 (headerP A.<?> "headerP")
  _ <- A.string "\r\n"
  body <- mrelPartBody bd
  pure (Map.fromList hs, body)

headerP :: A.Parser (B.ByteString, B.ByteString)
headerP = do
  h <- token
  _ <- A.char ':'
  _ <- A.char ' '
  v <- A.takeWhile1 (/= '\r')
  _ <- A.endOfLine
  pure (h, v)

mrelPartBody :: B.ByteString -> A.Parser B.ByteString
mrelPartBody boundary = do
  go (mempty :: B.Builder) ""
  where
    go bs nlcarry = do
      mbd <- A.option Nothing (Just <$> A.string boundary)
      case mbd of
        Just _ ->
          pure $ BL.toStrict $ B.toLazyByteString bs
        Nothing -> do
          line <- A.takeWhile1 (/= '\r')
          _ <- A.string "\r\n"
          go (bs <> nlcarry <> B.byteString line) "\r\n"

-- | Expects a multipart body, returns the result of parsing the first part as xml
processMprHeadXml
  :: B.ByteString -- ^ multipart boundry string
  -> B.ByteString -- ^ response body
  -> Either String Document
processMprHeadXml boundary wholeBody = do
  case A.parseOnly (mrelBody boundary) wholeBody of
    Right [(_headers, body)] -> do
      case parseLBS def $ BL.fromStrict body of
        Left err ->
          Left $ show err
        Right doc ->
          Right doc
    Right _ -> do
      Left "error: response has zero or multiple parts, expected exactly one"
    Left err -> do
      Left $ "error: " <> err

simpleParse
  :: B.ByteString -- ^ raw Content-Type header value
  -> B.ByteString -- ^ reponse body
  -> Either String Document
simpleParse ctype body =
  case parseContentType ctype of
    Right ("multipart/related", ps) -> do
      case getCtParams ps of
        Just (_, _, _, boundary) -> do
          processMprHeadXml boundary body
        Nothing -> do
          Left "error: could not parse Content-Type parameters"
    Right (x, _) -> do
      Left $ "error: unexpected Content-Type, wanted multipart/related, got: " <> show x
    Left err -> do
      Left $ "error: " <> err
  where
    getCtParams m = do
      t <- Map.lookup "type" m
      s <- Map.lookup "start" m
      b <- Map.lookup "boundary" m
      si <- Map.lookup "start-info" m
      pure (t, s, si, b)
