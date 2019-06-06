local it = require'examples.iter'

function odd(n : number)
   return n % 2 ~= 0
end

it.array<integer>{1,2,3,4}
   :map((i : integer) -> i*3)
   :filter(odd)
   :foreach(print)

it.rand(40)
   :drop(10)
   :take(20)
   :foreach(print)
