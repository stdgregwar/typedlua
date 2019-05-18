local function pair<U>(a : U, b : U)
   return a,b
end

local function a(i : {"a":integer})
   return true
end

local function b(i : {"a":integer, "b":string})
   return false
end

local c,d : number = 1,2

local function higher<S>(a : S, f : (S)->(S))
   return f(a)
end

local function higherrev<S>(f : (S)->(S), a : S)
   return f(a)
end

local function fpair<U,S>(arg1 : U, a : (U)->(S), b : (U)->(S), arg2 : U)
   return a(arg1), b(arg2)
end

-- width subtyping works
pair({c=1}, {c=1,a=2})

-- intersection of records via Lowest upper bound
pair({c=1, a=1}, {c=1, b=1})

-- even empty intersection works
pair({a=1},{b=1})

-- manual instance works
pair<{"c":integer}>({c=1, a=1}, {c=1, b=1})

-- complex inference with intersections and function involved
fpair({a=1,b='s',d='a'}, a, b, {a=1,b='s',c='a'})


higher({c=1, t=true}, (a : {'c':number}) -> {c=a.c,t=false,g="test"})

local fun = (a : {'c':number}) -> {c=a.c,t=false,g="test"}

higherrev(fun, {c=1, t=true})

-- define a function to turn U into Option<U>
local function Some<U>(v : U) : U?
   return v
end

-- Lowest upper bound for Union (T | nil in this case) gives merge of union
pair(Some('toto'), Some(24)) -- integer | string | nil

