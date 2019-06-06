local interface BoxedNumber
   val : number
   @__add : (BoxedNumber, BoxedNumber) -> (BoxedNumber)
end

-- hacky forward declaration
local BoxedNumber : any

local mt = {}

function mt.__add(r : BoxedNumber, l : BoxedNumber) : BoxedNumber
   return BoxedNumber(r.val+l.val)
end

function BoxedNumber(n : number) : BoxedNumber
   return setmetatable({val=n}, mt)
end

local one = BoxedNumber(1)
local two = BoxedNumber(2)

local three = one + two
