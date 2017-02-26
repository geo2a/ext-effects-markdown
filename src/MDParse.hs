{-# LANGUAGE FlexibleContexts
            , TypeOperators
            , ExistentialQuantification
            , ScopedTypeVariables 
            , TypeApplications 
            , ConstraintKinds
            , RankNTypes #-}

module MDParse where

import Control.Monad
import Control.Applicative hiding (some, many)

import Parsers

-----------------------------------------------------------------
---------------------------Ограничения---------------------------
-----------------------------------------------------------------
-- Всех не перечесть, парсим пока только заголовки и параграфы простого текста, 
-- а также списки; вложенные конструкции не допускаются 

-------------------Data Types-------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Line)
           | Paragraph [Line]
           | UnorderedList [Line]
           | BlockQuote [Line]
  deriving (Show,Eq)

-- |Represents line as list of inline elements (words)
data Line = Empty | NonEmpty [Inline]
  deriving (Show,Eq)

-- |Represent inline entity, just a string for this moment  
-- Что делать с пунктуацией и пробелами? 
data Inline = Plain String
            | Bold String
            | Italic String
            | Monospace String
  deriving (Show,Eq) 

-----------------------------------------------------------------
-------------------Parsers for Inline elements-------------------
-----------------------------------------------------------------

-------------------Helper Parsers-----------------
punctuation :: Parser r Char
punctuation = foldl1 alt (map char marks) 
  where marks = ".,:;!?'"

alphanumsWithPunctuation :: Parser r String
alphanumsWithPunctuation = some (alphanum `alt` punctuation)

-- |Sequence of alphanums with punctuations and spaces
sentence :: Parser r String
sentence =
  some (do
    w <- alphanumsWithPunctuation 
    s <- (many (char ' ')) 
    return $ w ++ s
  ) >>= pure . concat

-----------Parsers for single word (word may be plain, bold or italic)----------

-- |Parse plain text
plain :: Parser r Inline
plain = do 
  txt <- sentence
  return . Plain $ txt
  
-- |Parse italic text (html <em>)
italic :: Parser r Inline
italic = do  
  txt <- bracket (char '*') sentence (char '*') `alt`
         bracket (char '_') sentence (char '_')
  p   <- many punctuation
  return . Italic $ txt ++ p  

-- |Parse bold text (html <strong>)  
bold :: Parser r Inline
bold = do
  txt <- bracket asterisks sentence asterisks `alt` 
         bracket underlines sentence underlines
  p   <- many punctuation
  return . Bold $ txt ++ p  
  where
    asterisks = char '*' >> char '*'
    underlines = char '_' >> char '_'  

monospace :: Parser r Inline
monospace = do
  txt <- bracket (char '`') sentence (char '`')
  p   <- many punctuation
  return . Monospace $ txt ++ p
-- -----------------------------------------------------------------
-- -------------------Parser for Lines-------------------
-- -----------------------------------------------------------------

wspaceOrTab = (\x -> x == ' ' || x == '\t')

-- |Парсит слова (plain, bold, italic), разделённые одним пробелом
-- Ломается, если, например, внутри plain есть * 
line :: Parser r Line
line = emptyLine `alt` nonEmptyLine

emptyLine :: Parser r Line
emptyLine = many (sat wspaceOrTab) >> char '\n' >> return Empty

-- TODO: Получилось как-то сложно, подумать, как бы попроще
nonEmptyLine :: Parser r Line
nonEmptyLine = do
  many (sat wspaceOrTab)
  l <- sepby1 (bold `alt` italic `alt` plain `alt` monospace) (many (char ' '))
  many (sat wspaceOrTab)
  char '\n'
  return . NonEmpty $ l

-- -----------------------------------------------------------------
-- -------------------Parsers for Block elements--------------------
-- -----------------------------------------------------------------

-- |Parse header
-- поддерживаются только заголовки в стиле #
header :: Parser r Block 
header = do
  hashes <- token (some (char '#')) 
  text <- nonEmptyLine
  return $ Header (length hashes,text)

-- |Parse paragraph
paragraph :: Parser r Block
paragraph = do
  --l <- bracket emptyLine nonEmptyLine emptyLine
  l <- some nonEmptyLine
  return . Paragraph $ l

-- |Parse unordered list
unorderdList :: Parser r Block
unorderdList = do
  items <- some (token bullet >> line)
  return . UnorderedList $ items 
  where
    bullet :: Parser r Char
    bullet = char '*' `alt` char '+' `alt` char '-' >>= return

-- TODO: Эта функция делает почти тоже самое, что и emptyLine, 
-- TODO непонятно, как совместить их в одну, или, по крайней мере, 
-- TODO избежать дублирования кода
blank :: Parser r Block
blank = many (sat wspaceOrTab) >> char '\n' >> return Blank

-- |Parse blockquote
blockquote :: Parser r Block
blockquote = do
  lines <- some (token (char '>') >> line)
  return . BlockQuote $ lines  

-- |Черновик для латех-блоков
blockMath :: Parser r Block
blockMath = (bracket (string "$$") (some (sat (/= '$'))) (string "$$")) >>= 
  return . Paragraph . (: []) . NonEmpty . (: []) . Plain . 
    (\x -> "$$" ++ x ++ "$$") 

-- -----------------------------------------------------------------
-- -------------------Parsers for whole Document--------------------
-- -----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
doc :: Parser r Document
doc = do
  ls <- many block
  --a <- header
  --b <- blank
  --c <- paragraph
  return $ ls
  --return [a,b,c] 

block :: Parser r Block
block = 
  blank `alt` header `alt` paragraph `alt` 
  unorderdList `alt` blockquote `alt` blockMath