local interface List<E>
  head : E?
  tail : List<E>?
end

local function list<E>(t : {E}) : List<E>
      local function make_list(t : {E}, i : integer) : List<E>
            local e = t[i]
            if e then
               return {head = e, tail = make_list(t, i+1)}
            else
              return {}
            end
      end
      return make_list(t, 1)
end

local l = list<number>{1,2,3,4,5}
