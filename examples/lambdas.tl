local function normal(a : number, b :number)
  return a + b, a-b
end

local magic = (a : number, b : number) -> a+b, a-b

normal(2,10)
magic(2,10)