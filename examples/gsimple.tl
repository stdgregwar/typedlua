-- Simple generics tests


local function range(from : number, to : number?)
      if not to then
         from, to = 0, from
      end
      return function(to : number, current : number) : number?
             if current < to then return current + 1 end
      end, to-1, from-1
end

local function printnum(n : number) return print(n) end

for i in range(2, 5) do printnum(i) end

local function map<U, V>(array : {U}, f : (U) -> (V))
      local res : {V} = {}
      for i, v in ipairs(array) do
          res[i] = f(v)
      end
      return res
end

local us : {number}= {1,2,3,4,5}

-- type parameter are infered
local vs = map(us, tostring)

local dus = map(us, (n : number) -> n*2)
