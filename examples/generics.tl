
-- Test file for generics

--test that normal funcs still work
local function placebo(arg : number) : number
end

local function array<U>(... : U)
      local arr : {U} = {...}
      return arr
end

-- create a number array
local my_array = array<number>(1,2,3,4,5,6,7,8,9)
