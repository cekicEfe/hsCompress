module HuffmanTree (turnHuffmanTreeToDictionary, constructHuffmanTree) where

data HuffmanTree a
  = HuffmanTree
      { nodeVal :: Integer,
        leftTree :: HuffmanTree a,
        rightTree :: HuffmanTree a
      }
  | Occurence a Integer

turnHuffmanTreeToDictionary :: HuffmanTree a -> [(a, String)]
turnHuffmanTreeToDictionary = flip turnHuffmanTreeToDictionary' []

turnHuffmanTreeToDictionary' :: HuffmanTree a -> String -> [(a, String)]
turnHuffmanTreeToDictionary' (Occurence a _) stride = [(a, stride)]
turnHuffmanTreeToDictionary' (HuffmanTree _ lt rt) stride =
  turnHuffmanTreeToDictionary' lt (stride ++ ['0'])
    ++ turnHuffmanTreeToDictionary' rt (stride ++ ['1'])

constructHuffmanTree :: [(a, Integer)] -> Maybe (Either (a, Integer) (HuffmanTree a))
constructHuffmanTree xs = constructHuffmanTree' $ map Left xs

constructHuffmanTree' :: [Either (a, Integer) (HuffmanTree a)] -> Maybe (Either (a, Integer) (HuffmanTree a))
constructHuffmanTree' [] = Nothing
constructHuffmanTree' [Right tree] = Just (Right tree)
constructHuffmanTree' [Left (char, occ)] = Just (Left (char, occ))
constructHuffmanTree' (x : y : xs) =
  case x of
    Left a -> case y of
      Left b -> constructHuffmanTree' (insertProperlyTree (createHuffmanTreeNN a b) xs)
      Right b -> constructHuffmanTree' (insertProperlyTree (createHuffmanTreeNT a b) xs)
    Right a -> case y of
      Left b -> constructHuffmanTree' (insertProperlyTree (createHuffmanTreeTN a b) xs)
      Right b -> constructHuffmanTree' (insertProperlyTree (createHuffmanTreeTT a b) xs)

insertProperlyTree :: HuffmanTree a -> [Either (a, Integer) (HuffmanTree a)] -> [Either (a, Integer) (HuffmanTree a)]
insertProperlyTree tree [] = [Right tree]
insertProperlyTree tree1 (x : xs) =
  case x of
    Left xLeft ->
      if snd xLeft == getElem tree1
        then Left xLeft : Right tree1 : xs
        else
          if snd xLeft > getElem tree1
            then Right tree1 : Left xLeft : xs
            else Left xLeft : insertProperlyTree tree1 xs
    Right xRight ->
      if getElem xRight == getElem tree1
        then Right xRight : Right tree1 : xs
        else
          if getElem xRight > getElem tree1
            then Right tree1 : Right xRight : xs
            else Right xRight : insertProperlyTree tree1 xs

createHuffmanTreeNN :: (a, Integer) -> (a, Integer) -> HuffmanTree a
createHuffmanTreeNN (ele1, cnt1) (ele2, cnt2) =
  HuffmanTree
    { nodeVal = cnt1 + cnt2,
      leftTree = Occurence ele1 cnt1,
      rightTree = Occurence ele2 cnt2
    }

createHuffmanTreeNT :: (a, Integer) -> HuffmanTree a -> HuffmanTree a
createHuffmanTreeNT (ele1, cnt1) tree = HuffmanTree (cnt1 + getElem tree) (Occurence ele1 cnt1) tree

createHuffmanTreeTN :: HuffmanTree a -> (a, Integer) -> HuffmanTree a
createHuffmanTreeTN tree (ele1, cnt1) = HuffmanTree (cnt1 + getElem tree) tree (Occurence ele1 cnt1)

createHuffmanTreeTT :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
createHuffmanTreeTT tree1 tree2 = HuffmanTree {nodeVal = getElem tree1 + getElem tree2, leftTree = tree1, rightTree = tree2}

getElem :: HuffmanTree a -> Integer
getElem (HuffmanTree val _ _) = val
getElem (Occurence _ val) = val
