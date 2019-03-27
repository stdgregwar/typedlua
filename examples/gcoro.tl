local interface CoroHandle<I,O>
   resume : (I) -> (boolean, O)
   status : () -> (string)
end

local interface CoroThread<I,O>
   yield : (O) -> (I)
end

-- create a typed coroutine wrapper, supports only single argument exchanges
local function coroutine_create<I,O>(f : (CoroThread<I,O> ,I) -> (O)) : CoroHandle<I,O>
    local thread = coroutine.create(f)
    local first = true
    local coro_thread = {
        yield = function(o : O) : I
            return coroutine.yield(o)
        end
    }
    return {
      resume = function(i : I) : (boolean, O)
        if first then
           first = false
           -- we need to pass the coro thread on the first resume so that
           -- the function gets it as an argument, later resume should not give it
           return coroutine.resume(thread, coro_thread, i)
        else
           return coroutine.resume(thread, i)
        end
      end,
      status = function() : string
             return coroutine.status(thread)
      end
    }
end

local function check_string(n : string) print(n) end

local chandle = coroutine_create(
    function(thread : CoroThread<number,string>, n : number) : string
       while true do
          n = thread.yield(tostring(n))
       end
       return tostring(n)
    end
)

for i = 1,10 do
    local s, n = chandle.resume(i)
    check_string(n)
end