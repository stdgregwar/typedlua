
-- Test file for generics

--test that normal funcs still work
local function placebo(arg : number) : number
      return arg
end

local function make_pair<A,B>(a : A, b : B)
      return {first=a, second = b}
end

local function tagged_array<U,R>(first : U, ... : R) : {"tag" : U, R}
      return {tag=first, ...}
end

local one = placebo(1)

-- create a number array
local my_array = tagged_array<string,number>("hello", 42, 1291)
local my_second_array = tagged_array<number, string>(54, "Adieu", "Sezig")

local a_fun = tagged_array<string, number>
-- local my_array_two_steps = a_fun("hello", 42, 1291) makes compiler crash

local interface Person
      firstname:string
      lastname:string
end

typealias Array<U> = {U}

local my_pair = make_pair<string, Person>("director", {firstname="Albus", lastname="Dumbledore"})

print(one, my_array)