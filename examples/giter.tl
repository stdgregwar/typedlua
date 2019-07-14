local it = require'examples.iter'

function odd(n : number)
   return n % 2 ~= 0
end

local interface Person
   age : number
   greeting : string
   name : string
   last_name : string
   greet : (Person) => (Person)
   full_name : () => (string)
end

local function rand_int(i : integer, j : integer) : integer
   return math.tointeger(math.random(i,j)) or 0
end

local names : {string} = {"Jonh", "Hubert", "Edward","Alice"}
local last_names : {string} = {"Smith", "Reeves", "Mousorgsky", "von Bob"}
local greetings : {string} = {"Hi!", "Hello", "Cheers", "Best"}

local function person_feed() : Person
   local p = {
      age = math.random(20,60),
      name = names[rand_int(1,4)] or "unnamed",
      last_name = last_names[rand_int(1,4)] or "unnamed",
      greeting = greetings[rand_int(1,4)] or "Bye"
   }

   function p:full_name()
      return string.format("%s %s", self.name, self.last_name)
   end

   function p:greet(o : Person)
      print(string.format("%s (%d) says %s to %s",
                          self:full_name(),
                          self.age,
                          self.greeting,
                          o:full_name()))
      return o
   end

   return p
end

local function opt<U>(f : ()->(U)) : (() -> (U?))
   return f
end

local president = person_feed()

local interface Greeter
   greeting : string
   end



local pit = it.iter(opt(person_feed))
   -- :map((p : Greeter) -> p.greeting)
   :take(10)
   :foreach((p : Person) -> p:greet(president))


it.iter(opt(person_feed))
   :take(20)
   :reduce((a : Person, b : Person) -> a:greet(b))

it.array<integer>{1,2,3,4}
   :map((i : number) -> i*3)
   :filter(odd)
   :foreach(print)

it.rand(40)
   :drop(10)
   :take(20)
   :foreach(print)
