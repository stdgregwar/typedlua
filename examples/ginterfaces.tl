local interface Box<U>
  val : U
end

local interface Pair<A, B>
  first : A
  second : B
end

local typealias PairWithNum<B> = Pair<number,B>

local function box<U>(val : U) : Box<U>
      return {val = val}
end

local function check_num_pair(pair : PairWithNum<string>) : boolean
      return pair.first == tonumber(pair.second)
end

local function unbox<U>(box : Box<U>) : U
  return box.val
end

local function check_num(a : number) print(a) end
local val = unbox(box(42))
check_num(val)

