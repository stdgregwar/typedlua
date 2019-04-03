-- @author stdgregwar
-- safe_coroutine library test

local safe_coroutine = require'examples.gcoro_lib'

--------------------------------------------------------------------
-- first simple example as a producer, consumer, almost function-like
--------------------------------------------------------------------
local function check_string(n : string) print(n) end

local chandle = safe_coroutine.create(
   function(thread : CoroThread<number,string>, n : number) : string
       while true do
          n = thread.yield(tostring(n))
       end
       return tostring(n)
    end
)

for i = 1,10 do
    local _, n = chandle.resume(i)
    check_string(n)
end

--------------------------------------------------------------------
-- second exemple that use coroutine as iterator makers, lua example
-- at https://www.lua.org/pil/9.3.html
--------------------------------------------------------------------
-- permutation generator
local function permgen(thread : CoroThread<nil,{number}?> , a : {number}) : {number}?
   local function permgen(a : {number}, n : integer)
      if n == 0 then
         thread.yield(a)
      else
         for i=1,n do
            -- put i-th element as the last one
            a[n], a[i] = a[i], a[n]

            -- generate all permutations of the other elements
            permgen(a, n - 1)

            -- restore i-th element
            a[n], a[i] = a[i], a[n]
         end
      end
   end
   return permgen(a, #a)
end

local function perm(a : {number})
   return safe_coroutine.wrap(
      function(thread : CoroThread<nil,{number}?>,_ : nil) : {number}?
        permgen(thread, a)
   end)
end

local function check_num(n : number) io.write(n, ' ') end

for a in perm{1,2,3,4,5} do
   for _, e in ipairs(a) do
      check_num(e)
   end
   io.write('\n')
end
