local typealias Exp = {"tag":"Number", "val":number}
   | {"tag":"Add", "lhs":Exp, "rhs":Exp}
   | {"tag":"Sub", "lhs":Exp, "rhs":Exp}
   | {"tag":"Mul", "lhs":Exp, "rhs":Exp}
   | {"tag":"Div", "lhs":Exp, "rhs":Exp}

local function check_type(s : string)
end

local function simple(s : string | number) : number?
   if type(s) == 'string' then
      return tonumber(s)
   else
      return s
   end
end


local function eval (e: Exp ): number
   if e.tag == "Number" then
      return e.val
   elseif e.tag == "Add" then
      check_type(e)
      return eval(e.lhs) + eval(e.rhs)
   elseif e.tag == "Sub" then
      return eval(e.lhs) - eval(e.rhs)
   elseif e.tag == "Mul" then
      check_type(e)
      return eval(e.lhs) * eval(e.rhs)
   elseif e.tag == "Div" then
      check_type(e)
      return eval(e.lhs) / eval(e.rhs)
   end
end
