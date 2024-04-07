local x = io.read()
local p = 32
p = 23
if p == x then
    p = x + p
    print(p - 3 + x)
else
    print(p - x)
end