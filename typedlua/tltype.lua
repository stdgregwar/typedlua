--[[
This module implements Typed Lua tltype.
]]

if not table.unpack then table.unpack = unpack end

local tltype = {}

tltype.integer = false

-- literal types

-- Literal : (boolean|number|string) -> (type)
function tltype.Literal (l)
  return { tag = "TLiteral", [1] = l }
end

-- False : () -> (type)
function tltype.False ()
  return tltype.Literal(false)
end

-- True : () -> (type)
function tltype.True ()
  return tltype.Literal(true)
end

-- Num : (number) -> (type)
function tltype.Num (n)
  return tltype.Literal(n)
end

-- Str : (string) -> (type)
function tltype.Str (s)
  return tltype.Literal(s)
end

-- isLiteral : (type) -> (boolean)
function tltype.isLiteral (t)
  return t.tag == "TLiteral"
end

-- isFalse : (type) -> (boolean)
function tltype.isFalse (t)
  return tltype.isLiteral(t) and t[1] == false
end

-- isTrue : (type) -> (boolean)
function tltype.isTrue (t)
  return tltype.isLiteral(t) and t[1] == true
end

-- isNum : (type) -> (boolean)
function tltype.isNum (t)
  return tltype.isLiteral(t) and type(t[1]) == "number"
end

-- isFloat : (type) -> (boolean)
function tltype.isFloat (t)
  if _VERSION == "Lua 5.3" then
    return tltype.isLiteral(t) and math.type(t[1]) == "float"
  else
    return false
  end
end

-- isInt : (type) -> (boolean)
function tltype.isInt (t)
  if _VERSION == "Lua 5.3" then
    return tltype.isLiteral(t) and math.type(t[1]) == "integer"
  else
    return false
  end
end

-- isStr : (type) -> (boolean)
function tltype.isStr (t)
  return tltype.isLiteral(t) and type(t[1]) == "string"
end

function tltype.isProj (t)
  return t.tag == "TProj"
end

-- base types

-- Base : ("boolean"|"number"|"string") -> (type)
function tltype.Base (s)
  return { tag = "TBase", [1] = s }
end

-- Boolean : () -> (type)
function tltype.Boolean ()
  return tltype.Base("boolean")
end

-- Number : () -> (type)
function tltype.Number ()
  return tltype.Base("number")
end

-- String : () -> (type)
function tltype.String ()
  return tltype.Base("string")
end

-- Integer : (boolean?) -> (type)
function tltype.Integer (i)
  if i then return tltype.Base("integer") else return tltype.Base("number") end
end

-- isBase : (type) -> (boolean)
function tltype.isBase (t)
  return t.tag == "TBase"
end

-- isBoolean : (type) -> (boolean)
function tltype.isBoolean (t)
  return tltype.isBase(t) and t[1] == "boolean"
end

-- isNumber : (type) -> (boolean)
function tltype.isNumber (t)
  return tltype.isBase(t) and t[1] == "number"
end

-- isString : (type) -> (boolean)
function tltype.isString (t)
  return tltype.isBase(t) and t[1] == "string"
end

-- isInteger : (type) -> (boolean)
function tltype.isInteger (t)
  return tltype.isBase(t) and t[1] == "integer"
end

-- nil type

-- Nil : () -> (type)
function tltype.Nil ()
  return { tag = "TNil" }
end

-- isNil : (type) -> (boolean)
function tltype.isNil (t)
  return t.tag == "TNil"
end

-- value type, (top type)

-- Value : () -> (type)
function tltype.Value ()
  return { tag = "TValue" }
end

-- isValue : (type) -> (boolean)
function tltype.isValue (t)
  return t.tag == "TValue"
end

-- nothing type, (botton type)

function tltype.Nothing()
  return {tag = "TNothing"}
end

function tltype.isNothing(t)
  return t.tag == "TNothing"
end

-- dynamic type

-- Any : () -> (type)
function tltype.Any ()
  return { tag = "TAny" }
end

-- isAny : (type) -> (boolean)
function tltype.isAny (t)
  return t.tag == "TAny"
end

-- self type

-- Self : () -> (type)
function tltype.Self ()
  return { tag = "TSelf" }
end

-- isSelf : (type) -> (boolean)
function tltype.isSelf (t)
  return t.tag == "TSelf"
end

-- union types

-- Union : (type*) -> (type)
function tltype.Union (...)
  local l1 = {...}
  -- remove unions of unions
  local l2 = {}
  for i = 1, #l1 do
    if tltype.isUnion(l1[i]) or tltype.isUnionlist(l1[i]) then
      for j = 1, #l1[i] do
        table.insert(l2, l1[i][j])
      end
    else
      table.insert(l2, l1[i])
    end
  end
  if #l2 == 1 then -- short circuit
    return l2[1]
  end
  -- remove duplicates
  local l3 = {}
  for i = 1, #l2 do
    local enter = true
    for j = i + 1, #l2 do
      if tltype.subtype(l2[i], l2[j]) and tltype.subtype(l2[j], l2[i]) then
        enter = false
        break
      end
    end
    if enter then table.insert(l3, l2[i]) end
  end
  if #l3 == 1 then -- short circuit
    return l3[1]
  end
  -- simplify union
  local t = { tag = "TUnion" }
  for i = 1, #l3 do
    local enter = true
    for j = 1, #l3 do
      if i ~= j and not tltype.isAny(l3[i]) and tltype.consistent_subtype(l3[i], l3[j]) then
        enter = false
        break
      end
    end
    if enter then table.insert(t, l3[i]) end
  end
  if #t == 0 then
    return tltype.Void()
  elseif #t == 1 then
    return t[1]
  else
    if tltype.isTuple(t[1]) then
      t.tag = "TUnionlist"
    end
    return t
  end
end

-- isUnion : (type, type?) -> (boolean)
function tltype.isUnion (t1, t2)
  if not t2 then
    return t1.tag == "TUnion"
  else
    if t1.tag == "TUnion" then
      for _, v in ipairs(t1) do
        if tltype.subtype(t2, v) and tltype.subtype(v, t2) then
          return true
        end
      end
      return false
    else
      return false
    end
  end
end

-- filterUnion : (type, type) -> (type)
function tltype.filterUnion (u, t)
  if tltype.isUnion(u) then
    local l = {}
    for _, v in ipairs(u) do
      if not (tltype.subtype(t, v) and tltype.subtype(v, t)) then
        table.insert(l, v)
      end
    end
    return tltype.Union(table.unpack(l))
  else
    return u
  end
end

-- UnionNil : (type, true?) -> (type)
function tltype.UnionNil (t, is_union_nil)
  if is_union_nil then
    return tltype.Union(t, tltype.Nil())
  else
    return t
  end
end


function tltype.indexUnion(union, indice)
  -- cannot do anything if we don't know the key
  if not tltype.isLiteral(indice) then
    return false
  end

  -- check that all member are tables with same key
  local ct = tltype.Any()
  for _, member in ipairs(union) do
    if not tltype.isTable(member) then
      return false
    end

    local t = tltype.unfold(tltype.getField(indice, union))
    if tltype.consistent_subtype(t, ct) then
      ct = t
    elseif not tltype.consistent_subtype(ct, t) then
      return false -- types are not compatible
    end
  end
  return ct
end
-- vararg types

-- Vararg : (type) -> (type)
function tltype.Vararg (t)
  return { tag = "TVararg", [1] = t, name = t.name and t.name .. "*" }
end

-- isVararg : (type) -> (boolean)
function tltype.isVararg (t)
  return t.tag == "TVararg"
end

-- tuple types

-- Tuple : ({number:type}, true?) -> (type)
function tltype.Tuple (l, is_vararg)
  if is_vararg then
    l[#l] = tltype.Vararg(l[#l])
  end
  return { tag = "TTuple", table.unpack(l) }
end

-- void type

-- Void : () -> (type)
function tltype.Void ()
  return { tag = "TVoid" }
end

-- isVoid : (type) -> (boolean)
function tltype.isVoid (t)
  return t.tag == "TVoid"
end

-- inputTuple : (type?, boolean) -> (type)
function tltype.inputTuple (t, strict)
  if not strict then
    if not t then
      return tltype.Tuple({ tltype.Value() }, true)
    else
      if not tltype.isVararg(t[#t]) then
        table.insert(t, tltype.Vararg(tltype.Value()))
      end
      return t
    end
  else
    if not t then
      return tltype.Tuple({ tltype.Nil() }, true)
    else
      if not tltype.isVararg(t[#t]) then
        table.insert(t, tltype.Vararg(tltype.Nil()))
      end
      return t
    end
  end
end

-- outputTuple : (type?, boolean) -> (type)
function tltype.outputTuple (t)
  if not t then
    return tltype.Tuple({ tltype.Nil() }, true)
  else
    if not tltype.isVararg(t[#t]) then
      table.insert(t, tltype.Vararg(tltype.Nil()))
    end
    return t
  end
end

-- retType : (type, boolean) -> (type)
function tltype.retType (t)
  return tltype.outputTuple(tltype.Tuple({ t }))
end

-- isTuple : (type) -> (boolean)
function tltype.isTuple (t)
  return t.tag == "TTuple"
end

-- union of tuple types

-- Unionlist : (type*) -> (type)
function tltype.Unionlist (...)
  local t = tltype.Union(...)
  if tltype.isUnion(t) then t.tag = "TUnionlist" end
  return t
end

-- isUnionlist : (type) -> (boolean)
function tltype.isUnionlist (t)
  return t.tag == "TUnionlist"
end

function tltype.Proj(label, idx)
  return { tag = "TProj", label, idx }
end

-- UnionlistNil : (type, boolean?) -> (type)
function tltype.UnionlistNil (t, is_union_nil)
  if type(is_union_nil) == "boolean" then
    local u = tltype.Tuple({ tltype.Nil(), tltype.String() })
    return tltype.Unionlist(t, tltype.outputTuple(u))
  else
    return t
  end
end

-- function types

-- Function : (type, type, true?) -> (type)
function tltype.Function (t1, t2, is_method)
  if is_method then
    table.insert(t1, 1, tltype.Self())
  end
  return { tag = "TFunction", [1] = t1, [2] = t2 }
end

function tltype.isFunction (t)
  return t.tag == "TFunction"
end

function tltype.isMethod (t)
  if tltype.isFunction(t) then
    for _, v in ipairs(t[1]) do
      if tltype.isSelf(v) then return true end
    end
    return false
  else
    return false
  end
end

-- generic types

-- transform TVariable in TParameters
local function variables_to_parameters(type_params, t)
  local function subst(names, t)
    if type(t) ~= 'table' then
      return t
    end
    if tltype.isVariable(t) and names[t[1]] then
      -- change variable into a Parameter
      return tltype.Parameter(names[t[1]])
    end
    if t.type_args then -- if we have type parameter currying in interfaces or typealiases
      for i, ta in ipairs(t.type_args) do
        t.type_args[i] = subst(names, ta)
      end
    end
    for i, ts in ipairs(t) do
      t[i] = subst(names, ts)
    end
    return t
  end

  local names  = {}
  for _, id in ipairs(type_params) do
    names[id[1]] = id
  end
  return subst(names, t)
end

function tltype.Generic(type_params, template_type)
  return {tag = "TGeneric", type_params=type_params, [1]=template_type}
end

function tltype.makeGeneric(type_params, inner_type)
  return tltype.Generic(type_params.names, variables_to_parameters(type_params.names, inner_type))
end

function tltype.isGeneric(t)
  return t.tag == "TGeneric"
end

function tltype.isGenericFunction(t)
  return tltype.isGeneric(t) and
    tltype.isFunction(t[1])
end

-- table types

-- Field : (boolean, type, type) -> (field)
function tltype.Field (is_const, t1, t2, is_meta)
  return { tag = "TField", const = is_const, [1] = t1, [2] = t2, meta = is_meta}
end

-- isField : (field) -> (boolean)
function tltype.isField (f)
  return f.tag == "TField" and not f.const
end

-- isConstField : (field) -> (boolean)
function tltype.isConstField (f)
  return f.tag == "TField" and f.const
end

function tltype.isMetaField(f)
  return f.tag == "TField" and f.meta
end

-- ArrayField : (boolean, type) -> (field)
function tltype.ArrayField (i, t)
  return tltype.Field(false, tltype.Integer(i), t)
end

-- Table : (field*) -> (type)
function tltype.Table (...)
  return { tag = "TTable", ... }
end

-- isTable : (type) -> (boolean)
function tltype.isTable (t)
  return t.tag == "TTable"
end

-- getField : (type, type) -> (type)
function tltype.getField (f, t)
  if tltype.isTable(t) then
    for _, v in ipairs(t) do
      if tltype.consistent_subtype(f, v[1]) then
        return v[2]
      end
    end
    return tltype.Nil()
  else
    return tltype.Nil()
  end
end

-- fieldlist : ({ident}, type) -> (field*)
function tltype.fieldlist (idlist, t)
  local l = {}
  for _, v in ipairs(idlist) do
    table.insert(l, tltype.Field(v.const, tltype.Literal(v[1]), t, v.meta))
  end
  return table.unpack(l)
end

-- checkTypeDec : (string, type) -> (true)?
function tltype.checkTypeDec (n, t)
  local predef_type = {
    ["boolean"] = true,
    ["number"] = true,
    ["string"] = true,
    ["value"] = true,
    ["any"] = true,
    ["self"] = true,
    ["const"] = true,
  }
  if not predef_type[n] then
    if tltype.isTable(t) then
      local namelist = {}
      for _, v in ipairs(t) do
        local f1, f2 = v[1], v[2]
        if tltype.isStr(f1) then
          local name = f1[1]
          if not namelist[name] then
            namelist[name] = true
          else
            local msg = "attempt to redeclare field '%s'"
            return nil, string.format(msg, name)
          end
        end
      end
      return true
    elseif tltype.isUnion(t) then
      return true
    elseif t.type_args then
      return true -- if type is parametric, let's handle it later
    else
      return nil, "attempt to name a type that is not a table"
    end
  else
    local msg = "attempt to redeclare type '%s'"
    return nil, string.format(msg, n)
  end
end

-- type variables

-- Variable : (string) -> (type)
function tltype.Variable (name, type_args)
  return { tag = "TVariable", [1] = name , type_args=type_args}
end

-- isVariable : (type) -> (boolean)
function tltype.isVariable (t)
  return t.tag == "TVariable"
end

-- global type variables

-- GlobalVariable : (string) -> (type)
function tltype.GlobalVariable (env, name, pos, typeerror, namespace)
  return { tag = "TGlobalVariable", [1] = name, [2] = env, [3] = pos, [4] = typeerror, [5] = namespace }
end

-- isVariable : (type) -> (boolean)
function tltype.isGlobalVariable (t)
  return t.tag == "TGlobalVariable"
end

-- Parameter types
function tltype.Parameter(id)
  return {tag = "TParameter", name = id[1], id = id.id}
end

function tltype.isParameter(t)
  return t.tag == "TParameter"
end

-- recursive types

-- Recursive : (string, type) -> (type)
function tltype.Recursive (x, t)
  return { tag = "TRecursive", [1] = x, [2] = t }
end

-- isRecursive : (type) -> (boolean)
function tltype.isRecursive (t)
  return t.tag == "TRecursive"
end

local function unfold_recursive (tr, t)
  if tltype.isRecursive(t) then
    if t[1] ~= tr[1] then
      local r = tltype.Recursive(t[1], unfold_recursive(tr, t[2]))
      r.name = t.name
      return r
    else
      return t
    end
  elseif tltype.isLiteral(t) or
     tltype.isBase(t) or
     tltype.isNil(t) or
     tltype.isValue(t) or
     tltype.isAny(t) or
     tltype.isSelf(t) or
     tltype.isVoid(t) then
    return t
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
    local r = { tag = t.tag, name = t.name }
    for k, v in ipairs(t) do
      r[k] = unfold_recursive(tr, v)
    end
    return r
  elseif tltype.isFunction(t) then
    local r = tltype.Function(unfold_recursive(tr, t[1]), unfold_recursive(tr, t[2]), t[3])
    r.name = t.name
    return r
  elseif tltype.isTable(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.Field(v.const, v[1], unfold_recursive(tr, v[2]), v.meta))
    end
    local r = tltype.Table(table.unpack(l))
    r.unique = t.unique
    r.open = t.open
    return r
  elseif tltype.isVariable(t) then
    if t[1] == tr[1] then
      return tr
    else
      return t
    end
  elseif tltype.isVararg(t) then
    return tltype.Vararg(unfold_recursive(tr, t[1]))
  else
    return t
  end
end

function tltype.unfold (t)
  if tltype.isGlobalVariable(t) then
    local env, name = t[2], t[1]
    local tt = env.interface[name]
    if tt then
      return tt
    else
      local pos = t[3]
      local typeerror = t[4]
      local msg = "type alias '%s' is not defined"
      msg = string.format(msg, name)
      typeerror(env, "alias", msg, pos)
      return tltype.Nil()
    end
  elseif tltype.isRecursive(t) then
    return unfold_recursive(t, t[2])
  else
    return t
  end
end

local function check_recursive (t, name)
  if tltype.isLiteral(t) or
     tltype.isBase(t) or
     tltype.isNil(t) or
     tltype.isValue(t) or
     tltype.isAny(t) or
     tltype.isSelf(t) or
     tltype.isVoid(t) then
    return false
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) or
         tltype.isTuple(t) then
    for _, v in ipairs(t) do
      if check_recursive(v, name) then
        return true
      end
    end
    return false
  elseif tltype.isFunction(t) then
    return check_recursive(t[1], name) or check_recursive(t[2], name)
  elseif tltype.isTable(t) then
    for _, v in ipairs(t) do
      if check_recursive(v[2], name) then
        return true
      end
    end
    return false
  elseif tltype.isVariable(t) then
    return t[1] == name
  elseif tltype.isRecursive(t) then
    return check_recursive(t[2], name)
  elseif tltype.isVararg(t) then
    return check_recursive(t[1], name)
  else
    return false
  end
end

-- checkRecursive : (type, string) -> (boolean)
function tltype.checkRecursive (t, name)
  return check_recursive(t, name)
end

-- Primitive functions

function tltype.Prim (name)
  return { tag = "TPrim", [1] = name, [2] = tltype.primtypes[name] }
end

function tltype.isPrim (t)
  return t.tag == "TPrim"
end

-- subtyping and consistent-subtyping

local subtype

local function subtype_literal (env, t1, t2)
  if tltype.isLiteral(t1) and tltype.isLiteral(t2) then
    return t1[1] == t2[1]
  elseif tltype.isLiteral(t1) and tltype.isBase(t2) then
    if tltype.isBoolean(t2) then
      return tltype.isFalse(t1) or tltype.isTrue(t1)
    elseif tltype.isNumber(t2) then
      return tltype.isNum(t1)
    elseif tltype.isString(t2) then
      return tltype.isStr(t1)
    elseif tltype.isInteger(t2) then
      return tltype.isInt(t1)
    else
      return false
    end
  else
    return false
  end
end

local function subtype_base (env, t1, t2)
  if tltype.isBase(t1) and tltype.isBase(t2) then
    return t1[1] == t2[1] or (tltype.isInteger(t1) and tltype.isNumber(t2))
  else
    return false
  end
end

local function subtype_nil (env, t1, t2)
  return tltype.isNil(t1) and tltype.isNil(t2)
end

local function subtype_top (env, t1, t2)
  return tltype.isValue(t2)
end

local function subtype_any (env, t1, t2, relation)
  if relation == "<:" then
    return tltype.isAny(t1) and tltype.isAny(t2)
  else
    return tltype.isAny(t1) or tltype.isAny(t2)
  end
end

local function subtype_self (env, t1, t2)
  return tltype.isSelf(t1) and tltype.isSelf(t2)
end

local function subtype_union (env, t1, t2, relation)
  if tltype.isUnion(t1) then
    for _, v in ipairs(t1) do
      if not subtype(env, v, t2, relation) then
        return false
      end
    end
    return true
  elseif tltype.isUnion(t2) then
    for _, v in ipairs(t2) do
      if subtype(env, t1, v, relation) then
        return true
      end
    end
    return false
  else
    return false
  end
end

local function subtype_function (env, t1, t2, relation)
  if tltype.isFunction(t1) and tltype.isFunction(t2) then
    return subtype(env, t2[1], t1[1], relation) and subtype(env, t1[2], t2[2], relation)
  else
    return false
  end
end

local function subtype_field (env, f1, f2, relation)
  if tltype.isField(f1) and tltype.isField(f2) then
    return subtype(env, f2[1], f1[1], relation) and
           subtype(env, f1[2], f2[2], relation) and
           subtype(env, f2[2], f1[2], relation)
  elseif tltype.isField(f1) and tltype.isConstField(f2) then
    return subtype(env, f2[1], f1[1], relation) and
           subtype(env, f1[2], f2[2], relation)
  elseif tltype.isConstField(f1) and tltype.isConstField(f2) then
    return subtype(env, f2[1], f1[1], relation) and
           subtype(env, f1[2], f2[2], relation)
  else
    return false
  end
end

local function subtype_table (env, t1, t2, relation)
  if tltype.isTable(t1) and tltype.isTable(t2) then
    if t1.unique then
      local m, n = #t1, #t2
      local k, l = 0, {}
      for i = 1, m do
        for j = 1, n do
          if subtype(env, t1[i][1], t2[j][1], relation) then
            if subtype(env, t1[i][2], t2[j][2], relation) then
              if not l[j] then
                k = k + 1
                l[j] = true
              end
            else
              return false
            end
          end
        end
      end
      if k == n then
        return true
      else
        for j = 1, n do
          if not l[j] then
            if not subtype(env, tltype.Nil(), t2[j][2], relation) then
              return false
            end
          end
        end
      end
      return true
    elseif t1.open then
      local m, n = #t1, #t2
      local k, l = 0, {}
      for i = 1, m do
        for j = 1, n do
          if subtype(env, t1[i][1], t2[j][1], relation) then
            if subtype_field(env, t2[j], t1[i], relation) then
              if not l[j] then
                k = k + 1
                l[j] = true
              end
            else
              return false
            end
          end
        end
      end
      if k == n then
        return true
      else
        for j = 1, n do
          if not l[j] then
            if not subtype(env, tltype.Nil(), t2[j][2], relation) then
              return false
            end
          end
        end
      end
      return true
    else
      local m, n = #t1, #t2
      for i = 1, n do
        local subtype = false
        for j = 1, m do
          if subtype_field(env, t1[j], t2[i], relation) then
            subtype = true
            break
          end
        end
        if not subtype then return false end
      end
      return true
    end
  else
    return false
  end
end

local function subtype_global_variable(env, t1, t2, relation)
  if env[t1] and env[t1][t2] then return true end
  if env[t1] then
    env[t1][t2] = true
  else
    env[t1] = { [t2] = true }
  end
  if tltype.isGlobalVariable(t1) and tltype.isGlobalVariable(t2) then
    if subtype(env, tltype.unfold(t1), tltype.unfold(t2), relation) then
      return true
    else
      env[t1][t2] = nil
      return false
    end
  elseif tltype.isGlobalVariable(t1) then
    if subtype(env, tltype.unfold(t1), t2, relation) then
      return true
    else
      env[t1][t2] = nil
      return false
    end
  elseif tltype.isGlobalVariable(t2) then
    if subtype(env, t1, tltype.unfold(t2), relation) then
      return true
    else
      env[t1][t2] = nil
      return false
    end
  else
    env[t1][t2] = nil
    return false
  end
end

local function subtype_variable (env, t1, t2)
  if tltype.isVariable(t1) and tltype.isVariable(t2) then
    if t1[1] == t2[1] then
      return true
    end
    return env[t1[1] .."@".. t2[1]]
  else
    return false
  end
end

local function subtype_recursive (env, t1, t2, relation)
  if tltype.isRecursive(t1) and tltype.isRecursive(t2) then
    env[t1[1] .."@".. t2[1]] = true
    return subtype(env, t1[2], t2[2], relation)
  elseif tltype.isRecursive(t1) and not tltype.isRecursive(t2) then
    return subtype(env, tltype.unfold(t1), t2, relation)
  elseif not tltype.isRecursive(t1) and tltype.isRecursive(t2) then
    return subtype(env, t1, tltype.unfold(t2), relation)
  else
    return false
  end
end

local function subtype_tuple (env, t1, t2, relation)
  if tltype.isTuple(t1) and tltype.isTuple(t2) then
    local len1, len2 = #t1, #t2
    if len1 < len2 then
      if not tltype.isVararg(t1[len1]) then return false end
      local i = 1
      while i < len1 do
        if not subtype(env, t1[i], t2[i], relation) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len2 do
        if not subtype(env, t1[i], t2[j], relation) then
          return false
        end
        j = j + 1
      end
      return true
    elseif len1 > len2 then
      if not tltype.isVararg(t2[len2]) then return false end
      local i = 1
      while i < len2 do
        if not subtype(env, t1[i], t2[i], relation) then
          return false
        end
        i = i + 1
      end
      local j = i
      while j <= len1 do
        if not subtype(env, t1[j], t2[i], relation) then
          return false
        end
        j = j + 1
      end
      return true
    else
      for k, _ in ipairs(t1) do
        if not subtype(env, t1[k], t2[k], relation) then
          return false
        end
      end
      return true
    end
  else
    return false
  end
end

local function subtype_nothing(env, t1, t2)
  return tltype.isNothing(t1) -- don't care of t2 as long as t1 is bottom type
end

local function subtype_parameter(env, t1, t2)
  -- TODO take bounds into account
  if tltype.isParameter(t1) and tltype.isParameter(t2) then
    return t1.name == t2.name and t1.id == t2.id
  end
  return false
end

local function check_generic_structure(t1, t2, t1_pos, t2_pos)
  if type(t1) ~= 'table' or type(t2) ~= 'table' then
    return t1 == t2
  end
  if t1.tag ~= t2.tag then
    return
  end
  if tltype.isParameter(t1) and tltype.isParameter(t2) then
    if t1_pos[t1.id] ~= t2_pos[t2.id] then
      return
    end
  end
  for i, st in ipairs(t1) do
    if not check_generic_structure(st, t2[i], t1_pos, t2_pos) then
      return
    end
  end
  return true
end

local function subtype_generic(env, t1, t2)
  if tltype.isGeneric(t1) and tltype.isGeneric(t2) then
    local t1_pos = {}
    for i, v in ipairs(t1.type_params) do
      t1_pos[v.id] = i
    end
    local t2_pos = {}
    for i, v in ipairs(t2.type_params) do
      t2_pos[v.id] = i
    end
    return check_generic_structure(t1, t2, t1_pos, t2_pos)
  end
end


function subtype (env, t1, t2, relation)
  if tltype.isVoid(t1) and tltype.isVoid(t2) then
    return true
  elseif tltype.isProj(t1) and tltype.isProj(t2) then
    return t1[1] == t2[1] and t1[2] == t2[2]
  elseif tltype.isPrim(t1) and tltype.isPrim(t2) then
    return t1[1] == t2[1]
  elseif tltype.isPrim(t1) then
    return subtype(env, t1[2], t2, relation)
  elseif tltype.isUnionlist(t1) then
    for _, v in ipairs(t1) do
      if not subtype(env, v, t2, relation) then
        return false
      end
    end
    return true
  elseif tltype.isUnionlist(t2) then
    for _, v in ipairs(t2) do
      if subtype(env, t1, v, relation) then
        return true
      end
    end
    return false
  elseif tltype.isTuple(t1) and tltype.isTuple(t2) then
    return subtype_tuple(env, t1, t2, relation)
  elseif tltype.isTuple(t1) and not tltype.isTuple(t2) then
    return false
  elseif not tltype.isTuple(t1) and tltype.isTuple(t2) then
    return false
  elseif tltype.isVararg(t1) and tltype.isVararg(t2) then
    local t1_nil = tltype.Union(t1[1], tltype.Nil())
    local t2_nil = tltype.Union(t2[1], tltype.Nil())
    return subtype(env, t1_nil, t2_nil, relation)
  elseif tltype.isVararg(t1) and not tltype.isVararg(t2) then
    local t1_nil = tltype.Union(t1[1], tltype.Nil())
    return subtype(env, t1_nil, t2, relation)
  elseif not tltype.isVararg(t1) and tltype.isVararg(t2) then
    local t2_nil = tltype.Union(t2[1], tltype.Nil())
    return subtype(env, t1, t2_nil, relation)
  else
    return subtype_literal(env, t1, t2) or
           subtype_base(env, t1, t2) or
           subtype_nil(env, t1, t2) or
           subtype_top(env, t1, t2) or
           subtype_any(env, t1, t2, relation) or
           subtype_self(env, t1, t2) or
           subtype_union(env, t1, t2, relation) or
           subtype_function(env, t1, t2, relation) or
           subtype_table(env, t1, t2, relation) or
           subtype_variable(env, t1, t2) or
           subtype_generic(env, t1, t2) or
           subtype_parameter(env, t1, t2) or
           subtype_global_variable(env, t1, t2, relation) or
           subtype_recursive(env, t1, t2, relation) or
           subtype_nothing(env, t1, t2, relation)
  end
end

function tltype.subtype (t1, t2)
  return subtype({}, t1, t2, "<:")
end

function tltype.consistent_subtype (t1, t2)
  return subtype({}, t1, t2, "<~")
end

-- most general type

function tltype.general (t)
  if tltype.isFalse(t) or tltype.isTrue(t) then
    return tltype.Boolean()
  elseif tltype.isInt(t) and tltype.integer then
    return tltype.Integer(true)
  elseif tltype.isNum(t) then
    return tltype.Number()
  elseif tltype.isStr(t) then
    return tltype.String()
  elseif tltype.isUnion(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.general(v))
    end
    return tltype.Union(table.unpack(l))
  elseif tltype.isFunction(t) then
    return tltype.Function(tltype.general(t[1]), tltype.general(t[2]))
  elseif tltype.isTable(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.Field(v.const, v[1], tltype.general(v[2]), v.meta))
    end
    local n = tltype.Table(table.unpack(l))
    n.unique = t.unique
    n.open = t.open
    n.name = t.name
    return n
  elseif tltype.isTuple(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.general(v))
    end
    return tltype.Tuple(l)
  elseif tltype.isUnionlist(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.general(v))
    end
    return tltype.Unionlist(table.unpack(l))
  elseif tltype.isVararg(t) then
    return tltype.Vararg(tltype.general(t[1]))
  else
    return t
  end
end

-- first level type

local function resize_tuple (t, n)
  local tuple = { tag = "TTuple" }
  local vararg = t[#t][1]
  for i = 1, #t - 1 do
    tuple[i] = t[i]
  end
  for i = #t, n - 1 do
    if tltype.isNil(vararg) then
      tuple[i] = vararg
    else
      tuple[i] = tltype.Union(vararg, Nil)
    end
  end
  tuple[n] = tltype.Vararg(vararg)
  return tuple
end

function tltype.unionlist2tuple (t)
  local max = 1
  for i = 1, #t do
    if #t[i] > max then max = #t[i] end
  end
  local u = {}
  for i = 1, #t do
    if #t[i] < max then
      u[i] = resize_tuple(t[i], max)
    else
      u[i] = t[i]
    end
  end
  local l = {}
  for i = 1, #u do
    for j = 1, #u[i] do
      if not l[j] then l[j] = {} end
      table.insert(l[j], u[i][j])
    end
  end
  local n = { tag = "TTuple" }
  for i = 1, #l do
    n[i] = tltype.Union(table.unpack(l[i]))
  end
  if not tltype.isVararg(n[#n]) then
    n[#n + 1] = tltype.Vararg(tltype.Nil())
  end
  return n
end

function tltype.unionlist2union (t, i)
  if tltype.isTuple(t) then
    return t[i]
  end
  local l = {}
  for _, v in ipairs(t) do
    l[#l + 1] = v[i]
  end
  return tltype.Union(table.unpack(l))
end

function tltype.first (t)
  if tltype.isTuple(t) then
    return tltype.first(t[1])
  elseif tltype.isUnionlist(t) then
    local l = {}
    for _, v in ipairs(t) do
      table.insert(l, tltype.first(v))
    end
    return tltype.Union(table.unpack(l))
  elseif tltype.isVararg(t) then
    return tltype.Union(t[1], tltype.Nil())
  else
    return t
  end
end

-- tostring

local function dumptable(t)
  local out = {}
  for k, v in pairs(t) do
    if type(v) == "table" and v.tag then
      out[#out+1] = tostring(k) .. " = " .. tltype.tostring(v)
    elseif type(v) == "table" then
      out[#out+1] = tostring(k) .. " = " .. dumptable(v)
    else
      out[#out+1] = tostring(k) .. " = " .. tostring(v)
    end
  end
  return "{" .. table.concat(out, ", ") .. "}"
end


local function parlistToType(parlist)
  local l = {}
  for i, par in ipairs(parlist) do
    if par.tag == "Id" then
      l[i] = par[2]
    elseif par.tag == "Dots" then
      l[i] = tltype.Vararg(par[1]) -- take parameter type
    end
  end
  return tltype.Tuple(l)
end

-- type2str (type) -> (string)
local function type2str (t, n)
  n = n or 0
  if n <= 0 and t.name then
    return t.name
  elseif tltype.isPrim(t) then
    return "built-in function " .. t[1]
  elseif tltype.isTrue(t) or tltype.isFalse(t) or tltype.isNum(t) then
    return tostring(t[1])
  elseif tltype.isLiteral(t) then
    return string.format("%q", t[1])
  elseif tltype.isBase(t) then
    return t[1]
  elseif tltype.isNil(t) then
    return "nil"
  elseif tltype.isValue(t) then
    return "value"
  elseif tltype.isNothing(t) then
    return "nothing"
  elseif tltype.isAny(t) then
    return "any"
  elseif tltype.isSelf(t) then
    return "self"
  elseif tltype.isUnion(t) or
         tltype.isUnionlist(t) then
    local l = {}
    local nullable = ""
    for k, v in ipairs(t) do
      if tltype.isNil(v) then
        nullable = #t == 2 and "?" or " | nil"
      else
        l[#l+1] = type2str(v, n-1)
      end
    end
    table.sort(l)
    return table.concat(l, " | ") .. nullable
  elseif tltype.isFunction(t) then
    return type2str(t[1], n-1) .. " -> " .. type2str(t[2], n-1)
  elseif tltype.isGeneric(t) then
    local typeParList = t.type_params
    local tpstr = ""
    for _,name in ipairs(typeParList) do
      tpstr = tpstr .. name[1] .. ","
    end
    tpstr = tpstr:sub(0,-2)

    local inner_type = type2str(t[1], n-1)
    return string.format("<%s>%s",
                         tpstr,
                         inner_type)
  elseif tltype.isParameter(t) then
    return t.name
  elseif tltype.isTable(t) then
    --if t.interface then return t.interface end
    local l = {}
    for k, v in ipairs(t) do
      l[k] = type2str(v[1], n-1) .. ": " .. type2str(v[2], n-1)
      if tltype.isMetaField(v) then
        l[k] = '@' .. l[k]
      end
      if tltype.isConstField(v) then
        l[k] = "const " .. l[k]
      end
    end
    return "{" .. table.concat(l, ", ") .. "}"
  elseif tltype.isVariable(t) then
    if t.type_args then
      local l = {}
      for i, ta in ipairs(t.type_args) do
        table.insert(l,type2str(ta))
      end
      return string.format("%s<%s>",t[1],table.concat(l,", "))
    else
      return t[1]
    end
  elseif tltype.isGlobalVariable(t) then
    if t[5] then
      return t[5] .. "." .. t[1]
    else
      return t[1]
    end
  elseif tltype.isRecursive(t) then
    return t[1] .. "." .. type2str(t[2], n-1)
  elseif tltype.isVoid(t) then
    return "void"
  elseif tltype.isTuple(t) then
    local l = {}
    for i = 1, #t-1 do
      l[i] = type2str(t[i], n-1)
    end
    --if not tltype.isNil(t[#t][1]) then
    
    if tltype.isParameter(t[#t]) or not tltype.isNil(t[#t][1]) then
      l[#t] = type2str(t[#t], n-1)
    end
    return "(" .. table.concat(l, ", ") .. ")"
  elseif tltype.isVararg(t) then
    return type2str(t[1], n-1) .. "*"
  else
    error("trying to convert type to string but got " .. dumptable(t))
  end
end

-- tostring : (type) -> (string)
function tltype.tostring (t, n)
  return type2str(t, n)
end


-- LUB to gain more occasions to infer types
local function lowest_upper_bound(env, t1, t2, ignore_literal)
  local function is_numeric(t)
    return tltype.isNumber(t) or tltype.isInteger(t) or tltype.isNum(t)
  end

  
  if subtype(env, t1, t2) then
    return t2
  elseif subtype(env, t2, t1) then
    return t1
  elseif ignore_literal and (tltype.isLiteral(t1) or tltype.isLiteral(t2)) then
    return tltype.Value()
  elseif subtype(env, t1, tltype.Integer()) and subtype(env, t2, tltype.Integer()) then
    return tltype.Integer()
  elseif subtype(env, t1, tltype.Number()) and subtype(env, t2, tltype.Number()) then
    return tltype.Number()
  elseif subtype(env, t1, tltype.String()) and subtype(env, t2, tltype.String()) then
    return tltype.String()
  elseif tltype.isTable(t1) and tltype.isTable(t2) then
    local m, n = #t1, #t2
    local int = {}
    for i = 1, m do
      for j = 1, n do
        local k = lowest_upper_bound(env, t1[i][1], t2[j][1], true)
        local v = lowest_upper_bound(env, t1[i][2], t2[j][2])
        if not tltype.isValue(k) then
          table.insert(int, tltype.Field(t1[i].const or t2[j].const, k, v, t1[i].meta))
        end
      end
    end
    return tltype.Table(table.unpack(int))
  elseif tltype.isUnion(t1) and tltype.isUnion(t2) then
    return tltype.Union(t1, t2) -- merge both union in a bigger one
  end

  return tltype.Value()
end

-- generic type instance

local function param_subst(t, sub)
  if type(t) ~= 'table' then return t end
  
  if tltype.isParameter(t) then
    if sub[t.name] then
      return sub[t.name]
    end
  end

  for i, st in ipairs(t) do
    t[i] = param_subst(st, sub)
  end
  return t
end

local function deepcopy(orig)
  local orig_type = type(orig)
  local copy
  if orig_type == 'table' then
    copy = {}
    for orig_key, orig_value in next, orig, nil do
      copy[deepcopy(orig_key)] = deepcopy(orig_value)
    end
    setmetatable(copy, deepcopy(getmetatable(orig)))
  else -- number, string, boolean, etc
    copy = orig
  end
  return copy
end

function tltype.instanciate_generic(generic, args)
  assert(#args == #generic.type_params, "param arity mismatch")
  local subst = {}
  local type_params = generic.type_params
  for i, t in ipairs(args) do
    subst[type_params[i][1]] = t
  end
  return param_subst(deepcopy(generic[1]), subst)
end

-- type inference

function tltype.infer_params(env, type_params, ptype, given, pos)
  local infered_bounds = {}
  local failed = false

  local function reverse_relation(rel)
    return ({
      ['<:'] = ':>',
      [':>'] = '<:',
      ['⁼'] = '='
    })[rel]
  end

  local function combine_ctx(c1, c2)
    if c1 == '_' then
      return c2
    elseif c1 == c2 then
      return c1
    elseif c1 == '=' or c2 == '=' then
      return '='
    elseif c1 == '<:' or c2 == '<:' then
      return '<:'
    else
      return c1
    end
  end

  local function infer_step(ptype, given, relation) -- TODO add relation direction
    if type(ptype) ~= 'table' or type(given) ~= 'table' then
      if ptype ~= given then
        failed = true
      end
      return
    end
    if tltype.isParameter(ptype) then
      local id = ptype.id
      local bounds = infered_bounds[id]
      local lower = bounds.l
      local upper = bounds.u
      bounds.ctx = combine_ctx(bounds.ctx, relation)

      if relation == '<:' then
        if subtype(env, given, lower) then
          return
        elseif subtype(env, lower, given) then
          bounds.l = given
          return
        else
          bounds.l = lowest_upper_bound(env, lower, given)
          return
        end
      elseif relation == ':>' then
        if subtype(env, upper, given) then
          return
        elseif subtype(env, given, upper) then
          bounds.u = given
          return
        -- else
        --   bounds.u = type_intersection(env, given, upper)
        --   return
        end
      else -- invariant
        if subtype(env, lower, given) and subtype(env, given, upper) then -- if type is in bounds, update
          bounds.l, bounds.u = given, given
        end
      end
      -- error, cannot concile types
      tltype.typeerror(env, "inference", string.format(
                         "cannot concile type bound [%s <: %s <: %s] and %s (relation : '%s')",
                         type2str(lower), ptype.name, type2str(upper), type2str(given), relation), pos)
      failed = true
    elseif relation == '<:' and subtype(env, ptype, given) then
      return
    elseif relation == ':>' and subtype(env, given, ptype) then
      return
    elseif relation == '=' and subtype(env, given, ptype) and subtype(env, ptype, given) then
      return
    else
      if ptype.tag ~= given.tag then
        tltype.typeerror(env, "inference", string.format(
                           "cannot deduce types argument for %s from %s",
                           type2str(ptype), type2str(given)), pos)
        failed = true
        return
      end
      if tltype.isFunction(ptype) then
        infer_step(ptype[1],given[1], reverse_relation(relation))
        infer_step(ptype[2],given[2], relation)
        return
      else
        for i, pt in ipairs(ptype) do
          infer_step(pt, given[i], relation)
        end
      end
    end
  end

  -- fill subst with infinite bounds
  for i, id in ipairs(type_params) do
    infered_bounds[id.id] = {
      l = tltype.Nothing(),
      u = tltype.Value(),
      ctx = '_'
    }
  end

  infer_step(ptype, given, '<:')
  if failed then
    tltype.typeerror(env, "inference", string.format(
                      "could not infer types parameters from template %s and arguments %s",
                      type2str(ptype), type2str(given)), pos
    )
    return false
  end

  local function type_from_bounds(bounds)
    local ctx = bounds.ctx
    if subtype(env, bounds.l, bounds.u) then -- valid bounds
      if ctx == '<:' then
        return bounds.l
      elseif ctx == ':>' then
        return bounds.u
      elseif ctx == '=' and subtype(env, bounds.u, bounds.l) and subtype(env, bounds.l, bounds.u) then
        return bounds.u
      elseif ctx == '_' then -- bounds not updated....
        return nil -- TODO insufficient info message
      end
    end
    return nil
  end

  local types = {}
  -- deduce concrete types from bounds
  for i, id in ipairs(type_params) do
    local bounds = infered_bounds[id.id]
    local t = type_from_bounds(bounds)
    -- tltype.typeerror(env, "inference", string.format(
    --                    "bounds : [%s <: %s <: %s] ctx : '%s', chosen : %s", type2str(bounds.l),
    --                    id[1],
    --                    type2str(bounds.u), bounds.ctx,
    --                    type2str(t and t or tltype.Void())),
    --                  pos)
    if not t then -- TODO better error messages
      tltype.typeerror(env, "inference", string.format(
                         "not enough information to infer type variable %s", id[1]
                                                      ),pos)
      return nil
    end
    table.insert(types, t)
  end
  return tltype.Tuple(types)
end

function tltype.typeerror (env, tag, msg, pos)
  local function lineno (s, i)
    if i == 1 then return 1, 1 end
    local rest, num = s:sub(1,i):gsub("[^\n]*\n", "")
    local r = #rest
    return 1 + num, r ~= 0 and r or 1
  end

  local l, c = lineno(env.subject, pos)
  local error_msg = { tag = tag, filename = env.filename, msg = msg, l = l, c = c }
  for i, v in ipairs(env.messages) do
    if l < v.l or (l == v.l and c < v.c) then
      table.insert(env.messages, i, error_msg)
      return
    end
  end
  table.insert(env.messages, error_msg)
end

-- Built-in functions

local tanyany = tltype.Table(tltype.Field(false, tltype.Any(), tltype.Any()))

tltype.primtypes = {
  ["type"] = tltype.Function(tltype.inputTuple(tltype.Tuple{tltype.Value()}), tltype.outputTuple(tltype.Tuple{tltype.String()})),
  ["math_type"] = tltype.Function(tltype.inputTuple(tltype.Tuple{tltype.Value()}), tltype.outputTuple(tltype.Tuple{tltype.Union(tltype.String(),tltype.Nil())})),
  ["assert"] = tltype.Function(tltype.inputTuple(tltype.Tuple{tltype.Value(), tltype.Vararg(tltype.Value())}), tltype.outputTuple(tltype.Tuple{tltype.Vararg(tltype.Value())})),
  ["error"] = tltype.Function(tltype.inputTuple(tltype.Tuple{tltype.Value(), tltype.Union(tltype.Integer(), tltype.Nil())}), tltype.Void()),
  ["require"] = tltype.Function(tltype.inputTuple(tltype.Tuple{tltype.String()}), tltype.outputTuple(tltype.Tuple{tltype.Value()})),
  ["setmetatable"] = tltype.Function(tltype.inputTuple(tltype.Tuple{tanyany, tltype.Union(tanyany, tltype.Nil())}), tltype.outputTuple(tltype.Tuple{tanyany}))
}

return tltype
