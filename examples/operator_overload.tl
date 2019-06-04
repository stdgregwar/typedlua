local interface Vec
   x, y : number
   print : () => ()
   dot : (Vec) => (number)
   @__mul : (Vec, Vec) -> (number)
   @__add : (Vec, Vec) -> (Vec)
   @__sub : (Vec, Vec) -> (Vec)
end

local function cast<T>(v) : T
   return v
end

local vec_methods = {x=0.0, y=0.0}

function vec_methods:print()
   print(self.x, self.y)
end

function vec_methods:dot(other : Vec)
   return self.x*other.x + self.y*other.y
end

local vec_mt = {__index = vec_methods}

typealias Vecc = (number, number) -> (Vec)

local Vec : Vecc

function vec_mt.__sub(a : Vec, b : Vec)
   return cast<Vecc>(Vec)(a.x-b.x, a.y-b.y)
end

function vec_mt.__add(a : Vec, b : Vec)
   return cast<Vecc>(Vec)(a.x+b.x, a.y+b.y)
end

function vec_mt.__mul(a : Vec, b : Vec)
   return a:dot(b)
end

function Vec(x : number, y : number)
   return setmetatable({x=x, y=y}, vec_mt)
end

-- local function check(s : string) end
local v1 = Vec(2,1)
local v2 = Vec(1,2)

local v3 = v1 * v2

print(v3)
--v3:print()
--check(v1)
