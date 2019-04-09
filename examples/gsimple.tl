-- Simple generics tests

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

local ss : {string} = {'123', '253'}

local dss = map(ss, tonumber)
