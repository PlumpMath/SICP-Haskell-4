
sigma :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sigma f a next b = iter a 0
  where
    iter current acc | current > b = acc
                     | otherwise = iter (next current) (acc + f current)

simpsonIntegral :: (Integral a, Fractional b) => (b -> b) -> a -> a -> a -> b
simpsonIntegral f a b n = 1.0 * (h / 3) * (sigma simTerm 0 (1+) n)
  where
    h = fromIntegral (b - a) / fromIntegral n
    simTerm k = (yk k) * term
      where
        yk k = f (fromIntegral a + h * fromIntegral k)
        term =
          case k of
            0 -> 1
            1 -> 1
            otherwise -> if odd k then 4 else 2