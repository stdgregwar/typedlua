-- local lib = {}

-- function lib.g(meh : string)
--    lib.h(meh)
-- end

-- function lib.h(meh : string)
--    lib.g(meh)
-- end

--g forward declaration
local g : (string) -> (string)

local function check(s : string) end

local function f(meh : string) : ((value) -> (string))
   return tostring
end

g = f('meh')

check(g)

-- g = function(meh : string)
--    return 'str' --, f(meh)
-- end
