
-- Test file for generics

--test that normal funcs still work
local function placebo(arg : number) : number
      return arg
end

local function array<U>(first : U, ... : U) : {U}
      return {first, ...}
end

local one = placebo("string")

-- create a number array
local my_array = array<number>(1,2)

local other_array = array<string>("Hello", "world")

print(one, my_array)