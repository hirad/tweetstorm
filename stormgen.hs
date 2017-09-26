import Data.List
import Data.Maybe

data Tweet
  = Tweet {
      content :: String
    , index :: Int
    }
    deriving (Eq)

showTweet :: Tweet -> String
showTweet t = show $ (show $ index t) ++ "/ " ++ content t

instance Ord Tweet where
  (Tweet _ i1) `compare` (Tweet _ i2) = i1 `compare` i2

instance Show Tweet where
  show = showTweet

data State
  = State {
      currentIndex :: Int
    , currentWords :: [String]
    , currentLength :: Int
    , accTweets :: [Tweet]
    }
    deriving (Show)


step :: State -> String -> State
step state str =
  let indexLength = (length $ show $ currentIndex state) + 2
      wordLength = currentLength state + length str
      spaceLength = pred $ length $ currentWords state
      totalLength = indexLength + wordLength + spaceLength
  in
    if totalLength > 140 then
      State { currentIndex = currentIndex state + 1
            , currentWords = [str]
            , currentLength = 0
            , accTweets =
                accTweets state
                ++ [Tweet { content = unwords $ currentWords state
                          ,  index = currentIndex state
                          }]
            }
    else
      state {
              currentWords = currentWords state ++ [str]
            , currentLength = currentLength state + length str
            }

emptyState = State { currentIndex = 1, currentWords = [], currentLength = 0, accTweets = [] }

tweetStorm = allTweetsFrom . foldl' step emptyState . words
  where allTweetsFrom s = accTweets s ++ (maybeToList $ lastTweet (currentIndex s) (currentWords s))
        lastTweet _ [] = Nothing
        lastTweet idx wrds = Just Tweet { content = unwords wrds, index = idx }

