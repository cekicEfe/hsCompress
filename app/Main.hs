module Main (main) where

import HuffmanCode (decodeFileHuffman, encodeFileHuffman)

main :: IO ()
main = do
  encodeFileHuffman "test.txt"
  decodeFileHuffman "test.txt.comp.bin"
