{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


{- |
Module      : Data.Parser.Dependent
Description : 
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

A reference implementation of a
Contravariant Parser, with examples.

required packages:

* contravariant
* parsers
* attoparsec
| -}

module Data.Parser.Dependent where
import Data.Functor.Contravariant
import Text.Parser.Combinators
import Text.Parser.Token
import Data.Attoparsec.Text (Parser,parseOnly)
import Control.Applicative ((<|>))
import Text.Parser.Char
import Data.Text
import qualified Data.List as List

--------------------------------------------------
-- End Imports
--------------------------------------------------


--------------------------------------------------
-- ContraParser Definitions
--------------------------------------------------

-- | ContraParser, for parsing dependent uniquely on input
data ContraParser b a where
   ContraParser :: { getParser :: a ->  Parser b  } ->  ContraParser b a

instance Contravariant (ContraParser b) where
  contramap g (ContraParser p) = ContraParser (p <$> g)



--------------------------------------------------
-- Combinators
--------------------------------------------------


-- | convert a ContraParser into a parser
contraParse :: a -> ContraParser b a -> Parser b
contraParse a parseBuild = getParser parseBuild a

-- | convert a parser into a ContraParser
constContraParse :: Parser b -> ContraParser b a
constContraParse p = ContraParser (const p)

-- | Embed a parser inside a ContraParser
--   many times only one small part of data
-- is actually contravariant
-- embed makes defining the rest of the stuff easier
-- Rember that embed modifies the final encoding
-- and you cannot chain Contra Parsers with different
-- final encodings together.

embed  :: Parser (t -> b)  -> ContraParser t a -> ContraParser b a

embed p c = ContraParser (\a -> do                                 
                                f   <- p 
                                val <- getParser c a
                                return $ f val )





-- | run the contra parser with no continuation
-- return the result
contraParseOnly  ::                      initial
                   -> ContraParser final initial -> Text
                  -> Either String final
                  
contraParseOnly a p = parseOnly (contraParse a p)













--------------------------------------------------
-- Example
--------------------------------------------------


-- | A sample bit of data, notice that it is
--   easy to see that without tags, the result
--   could be very ambiguous
data Response = FinalInteger Integer | FinalInt Int
    deriving (Show,Eq,Ord)

data Request = RequestInteger | RequestInt
    deriving (Show,Eq,Ord)


-- | Decode a given string as an integer
decodeResponseInteger :: Parser Response
decodeResponseInteger = FinalInteger <$> integer


-- | Decode a given string as a n Int
decodeResponseInt :: Parser Response
decodeResponseInt = FinalInt . fromIntegral <$> integer

-- | The base function that explicitly shows
-- the dependent nature of the Parser
decodeByRequest :: Request -> Parser Response
decodeByRequest req =  case req of
                          RequestInt      ->   decodeResponseInt <?> "expected a responseInt"
                          RequestInteger  ->   decodeResponseInteger <?> "expected a responseInteger"



--------------------------------------------------
-- The Base
--------------------------------------------------

-- | wrap the function in a Contravariant Functor
-- 'ContraParser', the final output is the first
-- Parameter, followed by the expected initial state
-- For next two examples, this will be the dependent part.
contraParserBase :: ContraParser Response Request
contraParserBase = ContraParser decodeByRequest





--------------------------------------------------
-- One Piece of Dependent Data
--------------------------------------------------

-- | Here is an example where the response is parsed with a top level dependency
exampleOne :: Either String Response
exampleOne = contraParseOnly RequestInt contraParserBase  "3"




--------------------------------------------------
-- Dependent and independent data mixed
--------------------------------------------------

-- | Some device has a code which is determined with a 3 character
--   identifier, depending on the calling system.
--   it may return: bounded (Int) or unbounded (Integer) Integer type

data AddressResponse = AddressResponse {
                         deviceCode  :: Char,                         
                         deviceNumber :: Response
                      }
  deriving (Show)

-- | The only part of the 'AddressResponse' that is dependent on the
-- calling system is the deviceNumber
-- We don't want to have to reinvent every other parser in the world
-- so embed! 
contraParseAddressResponse :: ContraParser AddressResponse Request
contraParseAddressResponse = embed final contraParserBase
 where
  final = do
      c1 <- alphaNum <?> "expected digit"
      _  <- char ','
      return (AddressResponse c1) 




exampleTwo :: Either String AddressResponse
exampleTwo = contraParseOnly RequestInt contraParseAddressResponse "a,3"


--------------------------------------------------
-- lists of Dependent data
--------------------------------------------------


parseListOfAddressResponses :: [Request] -> Parser [AddressResponse]
parseListOfAddressResponses requests = sequence $ List.foldr decodeOneResponse [] requests
   where
     seperator = (char '|' *> pure ()) <|> eof
     decodeOneResponse req lst = let p =  (getParser contraParseAddressResponse req)
                                     in  (p <* seperator):lst



multiAddressContraParser :: ContraParser [AddressResponse] [Request]
multiAddressContraParser = ContraParser parseListOfAddressResponses


exampleThree :: Either String [AddressResponse]
exampleThree = contraParseOnly [RequestInt,RequestInteger,RequestInt] multiAddressContraParser "a,3|a,2|b,4"




--------------------------------------------------
-- Non top level sources
--------------------------------------------------

data AddressRequestFullMessage = AddressRequestFullMessage {
   requestName :: Text,
   requestArray :: [Request]
              }

exampleAddressRequestFullMessage :: AddressRequestFullMessage
exampleAddressRequestFullMessage = AddressRequestFullMessage
                                         "all pins"
                                         [RequestInt
                                         ,RequestInteger
                                         ,RequestInteger]


fullMessageResponse  :: ContraParser [AddressResponse] AddressRequestFullMessage
fullMessageResponse = contramap requestArray multiAddressContraParser                                         

exampleFour :: Either String [AddressResponse]
exampleFour = contraParseOnly exampleAddressRequestFullMessage fullMessageResponse "a,3|a,2|c,4"
