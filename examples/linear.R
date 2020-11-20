v <- validator( A %in% c("a1", "a2")
              , B %in% c("b1", "b2")
              , if (A == "a1") B == "b1"
              , y > x
              , z + 1 < 2*x + 3*y
              )

is_linear(v)
