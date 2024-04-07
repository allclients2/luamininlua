-- This LUA file was created with LuamininLua, a Smart Lua Formatter in Lua.

-- If statement
local x = 10
do
	print("x is positive")
end

-- While loop
local i = 1
while i <= 5 do
	print("While loop iteration:", i)
	i = i + 1
end
-- Repeat until loop
local j = 1
repeat
	print("Repeat until loop iteration:", j)
	j = j + 1
until j > 5

-- For loop
print("For loop:")
for k = 1, 5 do
	print(k)
end

-- Function set variable
local a = 23
a = 24
print(26)

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