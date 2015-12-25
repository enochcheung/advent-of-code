import Math.NumberTheory.Powers

coord n m = (div (k*(k+1)) 2)+m
        where
            k = n+m

solve n m = mod ((powerMod 252533 c 33554393) * 20151125) 33554393
        where
            c = coord (n-1) (m-1)

