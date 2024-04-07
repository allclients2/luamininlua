-- If statement
local x = 10
if x > 0 then
    print("x is positive")
elseif x < 0 then
    print("x is negative")
else
    print("x is zero")
end

-- While loop
local i = 1
while i <= 5 do --solving to `true`
    print("While loop iteration:", i)
    i = i + 1 --this is also solving to `i = 2`
end

-- Repeat until loop
local j = 1
repeat
    print("Repeat until loop iteration:", j)
    j = j + 1 --known problem is solving to `j = 2`, as no repeat loop support.
until j > 5 --this is solving to false

-- For loop
print("For loop:")
for k = 1, 5 do
    print(k)
end

-- Nested loop
print("Nested loops:")
for m = 1, 3 do
    for n = 1, 2 do
        print("m =", m, "n =", n)
    end
end

-- Break statement
print("Break statement:")
for p = 1, 10 do
    if p == 5 then
        break
    end
    print(p)
end
