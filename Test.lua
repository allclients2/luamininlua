-- Functions
function greet(name)
    print("Hello, " .. name .. "!")
end

-- Tables
fruits = {"apple", "banana", "cherry"}
print(fruits[3])
person = {name = "Alice", age = 30}
print(person["age"])

-- Loops
for i = 1, 5 do
    print("Count: " .. i)
end

for index, fruit in ipairs(fruits) do
    print(index .. ": " .. fruit)
end

-- Conditionals
age = 20
if age >= 18 then
    print("Adult")
else
    print("Minor")
end

-- Strings
message = "Hello, world!"
print(string.upper(message))

-- File I/O
file = io.open("test.txt", "w")
file:write("Hello, file I/O!")
file:close()

file = io.open("test.txt", "r")
content = file:read("*a")
print(content)
file:close()

-- Coroutines
function coroutine_func()
    for i = 1, 3 do
        print("Coroutine: " .. i)
        coroutine.yield()
    end
end

co = coroutine.create(coroutine_func)
coroutine.resume(co)
coroutine.resume(co)
coroutine.resume(co)

-- Metatables
--local proxysetmt = setmetatable
rectangle = {width = 10, height = 20}
proxysetmt(rectangle, {
	__index = function (a, b)
		return "lol"
	end
})
print(rectangle.width)
