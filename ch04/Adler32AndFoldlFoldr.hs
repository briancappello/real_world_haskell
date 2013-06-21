
my_sum :: (Num a) => [a] -> a
my_sum xs = loop 0 xs
    where loop :: (Num a) => a -> [a] -> a
          loop acc (x:xs) = loop (acc + x) xs
          loop acc []     = acc

-- Adler32 checksum: concatenates two 16bit checksums together
--   the first checksum is the sum of all input bytes, plus one
--   the second checksum is the sum of all intermediate values of the first
--   sums are computed modulo 65521

-- In Java it looks like this:
    -- public class Adler32 {
    --     private static final int base = 65521;
    --     public static int compute(byte[] data, int offset, int length) {
    --         int a = 1, b = 0;
    --         for (int i = offset; i < offset + length; i++) {
    --             a = (a + (data[i] & 0xff)) % base;
    --             b = (a + b) % base;
    --         }
    --         return (b << 16) | a;
    --     }
    -- }

-- And in Haskell: (see imports above)
base = 65521
adler32 :: [Char] -> Int
adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b []     = (b `shiftL` 16) .|. a

adler32_2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
        let a' = (a + (ord x .&. 0xff)) `mod` base
            b' = (a' + b) `mod` base
        in helper (a',b') xs
    helper (a,b) [] = (b `shiftL` 16) .|. a

