-- @author stdgregwar
-- This is a typed front end to lua coroutines

local safe_coroutine = {}

interface CoroHandle<I,O>
   resume : (I) -> (boolean, O)
   status : () -> (string)
end

interface CoroThread<I,O>
   yield : (O) -> (I)
end

-- create a typed coroutine wrapper, supports only single argument exchanges
function safe_coroutine.create<I,O>(f : (CoroThread<I,O> ,I) -> (O)) : CoroHandle<I,O>
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

function safe_coroutine.wrap<I,O>(f : (CoroThread<I,O>, I) -> (O)) : ((I) -> (O))
   local hand = safe_coroutine.create(f)
   return function(i : I)
      local code, data = hand.resume(i)
      -- TODO propagate error code
      return data
   end
end

return safe_coroutine
