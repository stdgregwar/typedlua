
-- Test file for generics

--test that normal funcs still work
local function placebo(arg : number) : number
      return arg
end

local function array<U>(... : U) : {U}
      return {...}
end

-- create a number array
local my_array = array<number>(1,2)
