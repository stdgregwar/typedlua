local function id<U>(arg : U) : U
      return arg
end

local function map<U, V>(array : {U}, f : (U) -> (V)) : {V}
      local res : {V} = {}
      for i, v in ipairs(array) do
          res[i] = f(v)
      end
      return res
end

local function boxmap<U, V>(array : {{"val":U}}, f : (U) -> (V)) : {V}
      local res : {V} = {}
      for i, v in ipairs(array) do
          res[i] = f(v.val)
      end
      return res
end

local us : {number}= {1,2,3,4,5}
local vs = map(us, tostring)

-- infer
local dus = map(us, (n : number) -> n*2)

local box : {{"val":number}}= {{val=1.1}}
local dbox = boxmap(box, (n : number) -> n*2)

local curry = id<string>

print(curry("Hello"))