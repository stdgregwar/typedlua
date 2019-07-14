interface Addable
    const __add:(Addable) => (self)
end

local function add(a : Addable, b : Addable)
      return a 
end