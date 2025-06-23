-- | This module allows parsing and generating cookie headers. You'll generally use
-- | the `Cookie.new` function to create a cookie from a name/value pair and the
-- | `Cookie.set*` functions to set attributes on a cookie.
-- |
-- | `Cookie.stringify` generates the string representation of a cookie, suitable for
-- | writing to an HTTP header.
-- |
-- | `Cookie.parse` parses the string representation of a cookie, returning an
-- | `Either ParseError Cookie`.
-- |
-- | ```purescript
-- | import PureStack.Cookie as Cookie
-- |
-- | > Cookie.stringify $ Cookie.setSecure $ Cookie.new "key" "value"
-- | key=value; Secure
-- |
-- | > Cookie.parse "key=value; Secure"
-- | (Right { name: "key", value: "value", secure: true, ... })
-- | ```
module PureStack.Cookie
  ( module PureStack.Cookie.Generator
  , module PureStack.Cookie.Parser
  , module PureStack.Cookie.Types
  ) where

import PureStack.Cookie.Generator (stringify)
import PureStack.Cookie.Parser (parse, parseMany)
import PureStack.Cookie.Types
  ( Cookie
  , SameSite(..)
  , _domain
  , _expires
  , _httpOnly
  , _maxAge
  , _name
  , _path
  , _secure
  , _value
  , expire
  , fromFields
  , getDomain
  , getExpires
  , getHttpOnly
  , getMaxAge
  , getName
  , getPath
  , getSecure
  , getValue
  , new
  , setDomain
  , setExpires
  , setHttpOnly
  , setMaxAge
  , setPath
  , setSameSite
  , setSecure
  )
