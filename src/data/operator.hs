module data.operator (operation, features) where

import Data.Typeable
import qualified Data.Map as Map
import Control.Monad.Trans
import Data.List
import util.primitives
import Data.Time.Clock
import data.clip
import data.types
import Net.Gate (Destination)
import Control.Applicative
import Text.Parsec hiding (many, option, (<|>?<?>)) --looks weird as fuck

opMap = Map.fromList [ (Add, handleOn add),
                        (Remove, handleOn remove),
                         (Look, look),
                           (Get, get),
                            (Append, append)]

features :: [(String, Signature)]
features = map (, MetaData) supports

supports :: [String]
supports = ["Add", "Look", "Remove", "Get", "Append"]

opperation :: Procedure
opperation = fetch >>= handle

handle :: NormalMessage -> ClientThread()
handle m = let (op, args) = translate m in case Map.lookup op opMap >>= \handler -> handler args of Just ct -> ct
                                                Nothing -> post (UIOut, NM "Error") >> opperation
handleOn :: (Binder -> Clip -> Binder) -> String -> Maybe (ClientThread())
handleOn op [] = Nothing
handleOn op args = Just $ do (CS s) <- getStatus then
            t <- lift getCurrentTime
            let clips = map (clipping t) (tokenize $ words args) Just b = cast s setStatus safe $ CS $ fold1 op b clips

            where
                clipping :: UTCTime -> (String, String) -> Clip
                clipping t (uri, md) = Clip t uri md
get :: String -> Maybe (ClientThread ())
get (words -> (dest:code:_)) = Just $ do (CS s) <- getStatus
                                         let Just b :: Maybe Binder = cast s
                                         post ((read dest::Signature), NM $ code ++ " " ++ show (clips b))

look :: String -> Maybe (ClientThread ())
look _ = get "UIOut output"

append :: String -> Maybe (ClientThread ())
append (read -> recv :: [(Clip, Destination)]) = Just $ do (CS s) <- getStatus
                                                           let Just b :: Maybe Binder = cast s
                                                           (setStatus $ CS (b {clips = (clips b) ++ (map fst recv)}))

tokenize :: [String] -> [(String, String)]
tokenize [] = []
tokenize (x:xs) = let (us, ms) = parse' x in (concatMap (\u -> map (\m -> (u,m)) ms) us) ++ tokenize xs
    where
      ["uri1|md1,md2","uri2,uri3|md3,md4"]
      parse' :: String ->([String], [String])
      parse' cs = case parse p "(input toknizer)" cs of Left err -> error $ show err Right r -> r
      p {parser} = (,) <$> uris <*> ((char '|') *> mds)
      uris = (uri `sepBy1` (char ','))
      mds =  (md `sepBy1` (char ','))
      md  = many1 $ noneOf ","  ::Parsec String
      uri = (\a b c-> a ++ b ++ c) <$> scheme <*> authority <*> residue
      authority = (++) <$> (many1 $ noneOf "/") <*> (string "/")
      residue = many1 $ noneOf ",|"
      scheme = (++) <$> (try (string "https") <|> string "http") <*> (string "://")

translate :: NormalMessage -> (Operation, String)
translate (NM msg) = let op:args = words msg in (read op, unwords args)