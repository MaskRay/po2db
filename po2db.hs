{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
import Prelude hiding (catch)
import Control.Applicative
import Control.Exception hiding (try)
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Lens.Common
import Data.Lens.Template
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Printf
import System.Console.CmdArgs
import System.IO
import System.IO.Unsafe

data Po2db = Po2db { dbFile :: FilePath
                   , poFiles :: [FilePath]
                   , tableName :: String
                   } deriving (Data, Typeable, Show, Eq)

data Head = Head { _translator :: String
                 , _translator_e :: String
                 , _team :: String
                 , _team_e :: String
                 , _charset :: String
                 , _plural :: String
                 } deriving (Show)
$(makeLenses [''Head])
emptyHead = Head "" "" "" "" "" ""

data Msg = Msg { _msgid :: [String]
               , _msgstr :: [String]
               , _msgctxt :: [String]
               , _msgflag :: [[String]]
               , _nmsgid :: Int
               , _nmsgctxt :: Int
               , _nmsgflag :: Int
               } deriving (Show)
$(makeLenses [''Msg])
emptyMsg = Msg [] [] [] [] 0 0 0

parseHash :: Parser (Msg -> Msg)
parseHash = (char '#' >>) . (<|> comment) $ do
  char ','
  flags <- sepBy (many1 (skipMany (char ' ') >> (letter_ascii <|> char '-'))) (char ',')
  return $ (msgflag ^!%= (flags:)) . (nmsgflag ^!%= succ)
  where
    comment = id <$ many (notChar '\n')

parseMsg :: Lens Msg [String] -> Lens Msg Int -> B.ByteString -> (Parser a) -> Parser (Msg -> Msg)
parseMsg l l_n t o = do
  string t
  option undefined o
  char ' '
  s <- (concat . concat) <$> many1 (char '"' *> many quotedChar <* char '"' <* char '\n')
  option undefined $ parseMsg l l_n t o
  return $ (l ^!%= (s:)) . (l_n ^!%= (+1))
  where
    quotedChar = (:[]) <$> try (satisfy (\c -> c /= '\\' && c /= '"')) <|> (char '\\' >> (\c -> ['\\',c]) <$> anyChar)

parseMsgid = parseMsg msgid nmsgid "msgid" (string "_plural")
parseMsgstr = parseMsg msgstr (lens (const 0) (flip const)) "msgstr" (char '[' *> digit <* char ']')
parseMsgctxt = parseMsg msgctxt nmsgctxt "msgctxt" (pure ())

parseHeader :: Parser (Head -> Head)
parseHeader = foldl1' (.) <$> sepBy (try p0 <|> try p1 <|> try p2 <|> try p3 <|> p4) (string "\\n")
  where
    pt0 :: Lens Head String -> Lens Head String -> B.ByteString -> Parser (Head -> Head)
    pt0 l l_e s = string s >> skipMany space >> many1 (notChar '<') >>= \t -> char '<' >> many1 (notChar '>') >>= \t_e -> return $ (l ^= rstrip t) . (l_e ^= t_e)
    p0 = pt0 translator translator_e "Last-Translator:"
    p1 = pt0 team team_e "Language-Team:"
    p2 = string "Content-Type: text/plain; charset=" >> (charset ^=) <$> many1 (notChar '\\')
    p3 = string "Plural-Forms: " >> (plural ^=) <$> many1 (notChar '\\')
    p4 = id <$ many1 (notChar '\\')
    rstrip = reverse . dropWhile (==' ') . reverse

parsePo :: Parser (Head, Msg)
parsePo = do
  fs <- (map $ foldl1' (.)) <$> sepBy (many1 (try parseMsgid <|> try parseMsgstr <|> try parseMsgctxt <|> parseHash)) (skipMany1 (char '\n'))
  let v = foldl' merge emptyMsg (reverse fs)
--  unsafePerformIO (zipWithM (\a b -> putStrLn "" >> putStrLn a >> putStrLn b) (v^.msgid) (concat (v^.msgflag))) `seq` return v
--  unsafePerformIO (print v) `seq` return v
  if null $ v ^. msgstr
    then fail "found no header"
    else case parse parseHeader . B.pack . head $ v^.msgstr of
    Fail _ pos err -> fail $ concat pos ++ err
    Partial _ -> fail $ show "placeholder"
    Done _ fh -> return (fh emptyHead, v)
  where
    merge :: Msg -> (Msg -> Msg) -> Msg
    merge acc g = p1 . p0 $ acc'
      where
        acc' = g acc
        p0 = if acc'^.nmsgid > acc'^.nmsgctxt then (msgctxt ^!%= ("":)) . (nmsgctxt ^!%= succ) else id
        p1 = if acc'^.nmsgid > acc'^.nmsgflag then (msgflag ^!%= ([]:)) . (nmsgflag ^!%= succ) else id

main = do
  options <- cmdArgs $ Po2db { dbFile = def &= argPos 0 &= typ "DB"
                           , poFiles = def &= args &= typ "PO1, ..."
                           , tableName = def &= typ "TABLE_NAME"
                           } &= help "Extract infomation from PO1, ... and insert into DB" &= details ["Table h_$(tableName) will be created to record (pof(PO filename) TEXT, lname(translator name) TEXT, lmail(translator's email) TEXT, tname(team name) TEXT, tmail(team's email) TEXT, charset TEXT, pforms(plural forms) TEXT).", "Table t_$(tableName) will be created to record (id INTEGER, msgid TEXT, msgstr TEXT, msgctxt TEXT, fuzzy(has fuzzy flag) bool, flag(other flags) TEXT, pof(PO filename) TEXT).", "If table t_$(tableName) already exists, it will be rename to t_$(tableName)_$(number)."]
  let [t, h, i_t, i_h] = map (++tableName options) ["t_", "h_", "i_t_", "i_h_"]

  conn <- connectSqlite3 $ dbFile options
  withTransaction conn $ \conn -> do
    let filt x = let r = stripPrefix t x
                     r' = fromJust r
                     rr = stripPrefix "_" r'
                     rr' = fromJust rr
                 in isJust r && (null r' || isJust rr && isNothing (find (`notElem` ['0'..'9']) rr'))
    ntables <- (length . filter filt) <$> getTables conn
    when (ntables > 0) $
      forM_ [h, t] $ \t -> run conn (printf "ALTER TABLE '%s' RENAME TO '%s_%d'" t t (ntables-1)) []

    run conn (printf "CREATE TABLE '%s' (pof TEXT,lname TEXT,lmail TEXT,tname TEXT,tmail TEXT,charset TEXT,pforms TEXT)" h) []
    run conn (printf "CREATE TABLE '%s' (id INTEGER,msgid TEXT,msgstr TEXT,msgctxt TEXT,fuzzy BOOL,flag TEXT,pof TEXT)" t) []

    stmtH <- prepare conn $ printf "INSERT INTO '%s' VALUES(?,?,?,?,?,?,?)" h
    stmtT <- prepare conn $ printf "INSERT INTO '%s' VALUES(?,?,?,?,?,?,?)" t
    
    forM_ (poFiles options) $ \f ->
      handle (\(SomeException e) -> hPutStrLn stderr (show e)) $ do
      contents <- do { h <- openFile f ReadMode; hSetEncoding h utf8; B.hGetContents h }
      
      let run header body = do
          execute stmtH $ map SqlString [f, header^.translator, header^.translator_e, header^.team, header^.team_e, header^.charset, header^.plural]
          executeMany stmtT $ zipWith5 (\i id_ str ctxt flag -> do
                                   let fuzzy = maybe "0" (const "1") $ find (=="fuzzy") flag
                                       flag' = delete "fuzzy" flag
                                   SqlInt64 i : map SqlString [id_, str, ctxt, fuzzy, intercalate "," flag', f]
                               ) [1..] (body^.msgid) (body^.msgstr) (body^.msgctxt) (body^.msgflag)
      
      case parse parsePo contents of
        Fail _ pos err -> hPutStrLn stderr $ concat pos ++ err
        Partial cont -> case cont "" of
          Done _ (header, body) -> run header body
        Done _ (header, body) -> run header body


    run conn (printf "CREATE INDEX '%s' on '%s' (pof,lname,lmail,tname,tmail,charset,pforms)" (if ntables > 0 then i_h++show (ntables-1) else i_h) h) []
    run conn (printf "CREATE INDEX '%s' on '%s' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)" (if ntables > 0 then i_t++show (ntables-1) else i_t) t) []

  disconnect conn