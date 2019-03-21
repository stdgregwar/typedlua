local function normal(a : number, b :number)
  return a + b, a-b
end

local magic = (a, b) -> a+b, a-b

magic(2,10)