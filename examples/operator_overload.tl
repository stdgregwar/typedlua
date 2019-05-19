local interface Vec
   x, y : number
   print : () => ()
   @__add : (Vec, Vec) -> (Vec)
end

local interface VecMt
   __add : (Vec, Vec) -> (Vec)
   const print : () => ()
end

local function cast<T>(v) : T
   return v
end


local vec_mt = {x=0.0, y=0.0}
vec_mt.__index = vec_mt

local Vec : (number, number) -> (Vec)

function vec_mt:print()
   print(self.x, self.y)
end

function vec_mt.__add(a : Vec, b : Vec) : Vec
   return setmetatable({x=a.x+b.x, y=a.y+b.y}, cast<VecMt>(vec_mt))
end

local function Vec(x : number, y : number) : Vec
   return setmetatable({x=x, y=y}, cast<VecMt>(vec_mt))
end

local function check(s : string) end
local v1 = Vec(0,0)
local v2 = Vec(1,2)

local v3 = v1 + v2

v1:print()
check(v3)
