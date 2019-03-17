
-- Test file for generics

--test that normal funcs still work
local function placebo(arg : number) : number
      return arg
end

local function array<U>(first : U, ... : U) : {U}
      return {...}
end

local one = placebo(1)

-- create a number array
local my_array = array<number>(1,2)
