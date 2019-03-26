local interface Box<U>
  val : U
end

local interface StrangeBox<U>
  val : U
  copy : U
end

local interface Pair<A, B>
  first : A
  second : B
end

local typealias PairWithNum<B> = Pair<number,B>

local function box<U>(val : U) : Box<U>
      return {val = val}
end

local function sbox<U>(val : U) : StrangeBox<U>
      return {val = val, copy = val}
end

local function check_num_pair(pair : PairWithNum<string>) : boolean
      return pair.first == tonumber(pair.second)
end

local function unbox<U>(box : Box<U>) : U
  return box.val
end

local is : {Box<number>} = {box(2.1), box(1.1)}
local ss : {StrangeBox<number>} = {sbox(2.2), sbox(3.3)}

local function map<U, V>(array : {U}, f : (U) -> (V))
      local res : {V} = {}
      for i, v in ipairs(array) do
          res[i] = f(v)
      end
      return res
end

local function sunbox<U>(e : StrangeBox<U>) : U return e.copy end

local is = map(is, sunbox<number>)
local ss = map(ss, unbox<number>)

local function check_num(a : number) print(a) end
local val = unbox(box(42))
check_num(val)

