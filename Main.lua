local LML = require("LuamininLua")

local result = LML.beautify ([[
    local x = 12 --haha
    local wow
    local function awe()
        wow()
    end
    function wow()
        x = 32 --troll
        print("hey")
        --local wow = "ew"
    end
    awe()
    print(x + 1) -- 33
]], true, true, true)

print("-----------------")
print(result)
print("-----------------")
print("Finished!")