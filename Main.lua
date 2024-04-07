local LML = require("LuamininLua")

io.input("Test.lua")

local testsrc = io.read("*a")

local result = LML.beautify(testsrc, false, true, true, true, true, false)

local output = io.open("output.lua", "w+")
output:write("-- This LUA file was created with LuamininLua, a Smart Lua Formatter in Lua.\n")
output:write(result)
output:flush()
output:close()

--print("-----------------")
--print(result)
--print("-----------------")
print("Finished!")