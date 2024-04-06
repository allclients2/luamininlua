local LML = require("LuamininLua")

io.input("Test.lua")

local testsrc = io.read("*a")

local result = LML.beautify(testsrc, false, true, true, true, true, true)

print("-----------------")
print(result)
print("-----------------")
print("Finished!")