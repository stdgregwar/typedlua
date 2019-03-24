local arr : {number} = {1,2,3,4}

local function numprint(n : number) print(n) end

for i, v in ipairs(arr) do numprint(v) end