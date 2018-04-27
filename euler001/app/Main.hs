multiples_sum max = (sum_of_multiples 3) + (sum_of_multiples 5) - (sum_of_multiples 15)
  where
    sum_of_multiples n = n * triang ((max-1) `quot` n)
    triang n = (n * (n + 1)) `quot` 2
    
main = putStrLn (show (multiples_sum 1000))
