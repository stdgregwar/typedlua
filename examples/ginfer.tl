local function pair<U>(a : U, b : U)
   return a,b
end

local function a(i : {"a":integer})
end

local function b(i : {"a":integer, "b":string})
end

-- width subtyping works
pair({c=1},{c=1,a=2})

-- not able to infer record intersection
pair({c=1, a=1}, {c=1, b=1})

-- manual instance works
pair<{"c":integer}>({c=1, a=1}, {c=1, b=1})

pair(a,b)

