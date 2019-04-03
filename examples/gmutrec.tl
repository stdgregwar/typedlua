local lib = {}

function lib.g(meh : string)
   lib.h(meh)
end

function lib.h(meh : string)
   lib.g(meh)
end

--g forward declaration
local g : (string) -> (string)

local function f(meh : string)
   return (g or function(n : string) return 'tata' end)(meh)
end

function g(meh : string)
   return f(meh)
end

