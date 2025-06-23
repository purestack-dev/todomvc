-- | This module is responsible for parsing a string into an
-- | `Either ParseError Cookie`.
module PureStack.Cookie.Parser
  ( parse
  , parseMany
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as String
import Parsing (ParseError, Parser, fail, runParser)
import Parsing.Combinators (many, sepBy)
import Parsing.String (eof, string)
import Parsing.String.Basic (noneOf)
import PureStack.Cookie.Formatter (domainTag, expiresTag, httpOnlyTag, maxAgeTag, pathTag, sameSiteTag, secureTag, unformatDateTime)
import PureStack.Cookie.Types (Cookie, SameSite(..))
import PureStack.Cookie.Types as Cookie

type Attribute = Cookie -> Cookie

dropSeperator :: Parser String Unit
dropSeperator = (string "; " $> unit) <|> (pure unit)

asString :: forall f. Functor f => Foldable f => f Char -> String
asString = String.fromCodePointArray <<< Array.fromFoldable <<< map String.codePointFromChar

stringWithout :: Array Char -> Parser String String
stringWithout = map asString <<< many <<< noneOf

attributeValue :: Parser String String
attributeValue = stringWithout [ ';' ]

whitespaceChars :: Array Char
whitespaceChars = [ ' ', '\n', '\r', '\t' ]

parseDomain :: Parser String Attribute
parseDomain = do
  domain <- (string $ domainTag <> "=") *> attributeValue
  pure $ Cookie.setDomain domain

parsePath :: Parser String Attribute
parsePath = do
  path <- (string $ pathTag <> "=") *> attributeValue
  pure $ Cookie.setPath path

parseExpires :: Parser String Attribute
parseExpires = do
  expiresString <- (string $ expiresTag <> "=") *> attributeValue
  case unformatDateTime expiresString of
    Left e -> fail e
    Right expires -> pure $ Cookie.setExpires expires

parseMaxAge :: Parser String Attribute
parseMaxAge = do
  maxAgeString <- (string $ maxAgeTag <> "=") *> attributeValue
  case Int.fromString maxAgeString of
    Nothing -> fail "invalid integer"
    Just maxAge -> pure $ Cookie.setMaxAge maxAge

parseSecure :: Parser String Attribute
parseSecure = (string secureTag) $> Cookie.setSecure

parseHttpOnly :: Parser String Attribute
parseHttpOnly = (string httpOnlyTag) $> Cookie.setHttpOnly

parseSameSite :: Parser String Attribute
parseSameSite = do
  sameSiteString <- (string $ sameSiteTag <> "=") *> attributeValue
  case sameSiteString of
    "Strict" -> pure $ Cookie.setSameSite Strict
    "Lax" -> pure $ Cookie.setSameSite Lax
    "None" -> pure $ Cookie.setSameSite None
    val -> fail $ "unknown SiteSite value " <> val

parseAttribute :: Parser String Attribute
parseAttribute =
  parseDomain
    <|> parsePath
    <|> parseExpires
    <|> parseMaxAge
    <|> parseSecure
    <|> parseHttpOnly
    <|> parseSameSite

parseSimpleCookie :: Parser String Cookie
parseSimpleCookie = do
  name <- stringWithout ([ ';', ',', '=' ] <> whitespaceChars) <* string "="
  value <- stringWithout ([ ';', ',' ] <> whitespaceChars)
  pure $ Cookie.new name value

parseCookie :: Parser String Cookie
parseCookie = do
  cookie <- parseSimpleCookie <* dropSeperator
  attributes <- sepBy parseAttribute (string "; ") <* eof
  pure $ foldl (#) cookie attributes

parseCookies :: Parser String (List Cookie)
parseCookies = sepBy parseSimpleCookie (string "; ") <* eof

-- | Parses a `String` into an `Either ParseError Cookie`. This
-- | function is primarily intended to parse the `Set-Cookie`
-- | header on the client.
-- |
-- | ```purescript
-- | > Parser.parse "key=value; Secure"
-- | (Right { name: "key", value: "value", secure: true, ... })
-- | ```
parse :: String -> Either ParseError Cookie
parse = flip runParser parseCookie

-- | Parses a `String` into an `Either ParseError (List Cookie)`.
-- | HTTP requests can include multiple cookies in a single
-- | `Cookie` header consisting of only name/value pairs. This
-- | function can be used to parse this header.
-- |
-- | ```purescript
-- | > parseMany "key1=value1; key2=value2"
-- | (Right ({ name: "key1", value: "value1", ... } : { name: "key2", value: "value2", ... } : Nil))
-- | ```
parseMany :: String -> Either ParseError (List Cookie)
parseMany = flip runParser parseCookies
