{--

@onelharrison [Twitter | Medium]

Description
-----------
Run-length encoding is a simple lossless data compression algorithm.
The basic idea is to reduce sequences of the same data (a run) to
a count and the data value of the run.

Implement run-length encoding and decoding for strings.

Constraints
-----------
 * unencoded input strings do not have characters from {0..9}
 * unencoded input strings only have characters from {a..zA..Z}
 * encoded input strings are always valid

Example
-------
 * unencoded input string = "WBBWWBB"   , output = "1W2B2W2B"
 * unencoded input string = "ABC"       , output = "1A1B1C"
 * unencoded input string = "ABC"       , output = "ABC" (BONUS)
 * encoded input string   = "4A3B2C1D2A", output = "AAAABBBCCDAA"

--}
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

runLengthEncode :: String -> String
runLengthEncode = concat . map compressRun . List.group

compressRun :: String -> String
compressRun xs
  | length xs == 1 = xs -- BONUS
  | otherwise = (show . length $ xs) ++ [head xs]

runLengthDecode :: String -> String
runLengthDecode xs = concat . map decompressRun $ compressedRuns
  where
    compressedRuns =
      Split.split
        (Split.dropFinalBlank . Split.keepDelimsR . Split.whenElt $
         Char.isLetter)
        xs

decompressRun :: String -> String
decompressRun compressedRun
  | length compressedRun == 1 = compressedRun -- BONUS
  | otherwise = replicate (read . init $ compressedRun) (last compressedRun)
