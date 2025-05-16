module HuffmanCode (encodeFileHuffman, decodeFileHuffman) where

import Data.Binary (Word8)
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Tuple (swap)
import GHC.Bits (setBit, testBit)
import HuffmanTree (constructHuffmanTree, turnHuffmanTreeToDictionary)
import Text.Read (readMaybe)

-- * Very Important

decodeFileHuffman :: FilePath -> IO ()
decodeFileHuffman binPath = do
  binFileContent <- BL.readFile binPath :: IO BL.ByteString
  dictFileContent <- readFile (binPath ++ ".dict")
  let maybeDict = readMaybe dictFileContent :: Maybe ([(Char, String)], Int)
  case maybeDict of
    Nothing -> print "File is not parsed right this is a no no"
    Just dict -> do
      let fileByteStr = BL.unpack binFileContent
      let decodedByteStr = decodeByteStringToBinStr fileByteStr (snd dict)
      let uncompressed = substituteStr [] decodedByteStr $ fst dict
      writeFile (binPath ++ ".uncompressed") uncompressed

-- Just horrible fix this at once mfer

substituteStr :: String -> String -> [(Char, String)] -> String
substituteStr _ [] _ = []
substituteStr [] (x : xs) dict = substituteStr [x] xs dict
substituteStr str (b : bl) dict = case (\x -> lookup x . map swap) str dict of
  Nothing -> substituteStr (str ++ [b]) bl dict
  Just a -> a : substituteStr [b] bl dict

-- * Very Important

encodeFileHuffman :: FilePath -> IO ()
encodeFileHuffman path =
  do
    fileContent <- readFile path
    let maybeHuffmanTree = constructHuffmanTree . turnBSTtoSortedListByOccurence $ getCharRateBST fileContent

    case maybeHuffmanTree of
      Nothing -> print "input file is empty ! or a problem occured ... sorry bye !"
      Just huffmanTree' -> case huffmanTree' of
        Left singleVowelContent -> print singleVowelContent
        Right justHuffmanTree -> do
          let dict = turnHuffmanTreeToDictionary justHuffmanTree
          let (compressedFile, expectedSize) = compressInit dict fileContent

          let strCompressedPath = path ++ ".comp.bin"
          let strDictPath = path ++ ".comp.bin.dict"

          BL.writeFile strCompressedPath compressedFile
          writeFile strDictPath $ show (dict, expectedSize)

-------------------------
-- Used while decoding --
-------------------------

decodeByteStringToBinStr :: [Word8] -> Int -> String
decodeByteStringToBinStr (x : xs) maxSize = word8toBinStr x 7 maxSize ++ decodeByteStringToBinStr xs (maxSize - 8)
decodeByteStringToBinStr [] _ = ""

-- * Important

word8toBinStr :: Word8 -> Int -> Int -> String
word8toBinStr _ z _
  | z < 0 = []
word8toBinStr _ _ z
  | z <= 0 = []
word8toBinStr word currentBit maxBit
  | not $ testBit word currentBit = '0' : word8toBinStr word (currentBit - 1) (maxBit - 1)
  | otherwise = '1' : word8toBinStr word (currentBit - 1) (maxBit - 1)

-- * Important

compressInit :: [(Char, String)] -> String -> (BL.ByteString, Int)
compressInit dict str = (intListToByteStr $ turnStrToInt decodedStr, expectedSize)
  where
    decodedStr = encodeStr dict str
    expectedSize = length decodedStr

-- * Important

encodeStr :: [(Char, String)] -> String -> String
encodeStr _ [] = []
encodeStr dict (x : rest) = case lookup x dict of
  Nothing -> []
  Just str -> str ++ encodeStr dict rest

-- * Important

intListToByteStr :: [Int] -> BL.ByteString
intListToByteStr ls = BL.pack (map fromIntegral ls)

-- * Important

turnStrToInt :: String -> [Int]
turnStrToInt [] = []
turnStrToInt str = binStrToWord8 (padStr leftSplit) 0 7 : turnStrToInt rest
  where
    (leftSplit, rest) = splitAt 8 str
    padStr string
      | length string < 8 = string ++ ['0']
      | otherwise = string

-- * Important

binStrToWord8 :: String -> Int -> Int -> Int
binStrToWord8 [] acc _ = acc
binStrToWord8 (s : ls) acc pos =
  if s == '1'
    then binStrToWord8 ls (setBit acc pos) (pos - 1)
    else binStrToWord8 ls acc (pos - 1)

----------------------------------------
-- Helper functions while compressing --
----------------------------------------

data BST a = EmptyBST | Node (CharOccurence a) (BST a) (BST a)

type CharOccurence a = (a, Integer)

turnBSTtoSortedListByOccurence :: BST a -> [CharOccurence a]
turnBSTtoSortedListByOccurence ls = sortBy compareByVal (turnBSTtoSortedListByOccurence' ls)
  where
    compareByVal (_, x2) (_, y2) = compare x2 y2

turnBSTtoSortedListByOccurence' :: BST a -> [CharOccurence a]
turnBSTtoSortedListByOccurence' EmptyBST = []
turnBSTtoSortedListByOccurence' (Node val lt gt) = val : turnBSTtoSortedListByOccurence' lt ++ turnBSTtoSortedListByOccurence' gt

getCharRateBST :: String -> BST Char
-- getCharRateList [] = EmptyBST
-- getCharRateList (x:xs) = diffuseToBst (CharOccurence x 1) (getCharRateList xs)
getCharRateBST = foldr (\x -> diffuseToBst (x, 1)) EmptyBST -- Better version

diffuseToBst :: (Ord a) => CharOccurence a -> BST a -> BST a
diffuseToBst occurence EmptyBST = Node occurence EmptyBST EmptyBST
diffuseToBst (newChar, newOcc) (Node (char, occ) lt gt)
  | newChar == char = Node (char, newOcc + occ) lt gt
  | newChar < char = Node (char, occ) (diffuseToBst (newChar, newOcc) lt) gt
  | newChar > char = Node (char, occ) lt (diffuseToBst (newChar, newOcc) gt)
diffuseToBst (_, _) (Node (a, b) c d) = Node (a, b) c d

-- diffuseToBst (_, _) _ = Node {} -- This will be never reached but compiler complains anyway
