-- LuamininLua.lua

--[[

MIT License

Copyright (c) 2017 Mark Langen
Copyright (c) 2024 all_clients (ROBLOX Userid: 852643438)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

]]

--#region Debug IO

local debugmode = false

local function dump(o, m, i) --fast dump
    local i, m, d = (i or 1), (m or 3), 3
    if type(o) == 'table' and i <= m then
        local f = string.rep(" ", i * d)
        local s = '{\n'
        for k, v in pairs(o) do
            if type(k) ~= 'number' then k = '"' .. tostring(k) .. '"' end
            s = s .. f ..  '[' .. k .. '] = ' .. dump(v, m, i + 1) .. ',\n'
        end
        return s .. f:sub(1, -1 - d) .. '}'
    else
        return tonumber(o) and o or ('"' .. tostring(o) .. '"')
    end
end

local dbgprint = debugmode and print or function (...) end

local dbgprinttab = debugmode and function(a, b)
    if _VERSION == "Luau" then
        dbgprint(a)
    else
        dbgprint(dump(a, b or 3))
    end
end or function (...) end

--#endregion

--#region Helper functions

local function clone(a)
    local b = {}
    for i,v in pairs(a) do
        b[i] = v
    end
    return b
end

local function clonedeep(a, seen)
    local seen = seen or {}
    local b, t = {}, {}
    for i,v in pairs(a) do
        seen[v] = true
        if type(v) == "table" and not seen[v] then
            t[i] = v
        end
        b[i] = v
    end
    for i,v in pairs(t) do
        b[i] = clonedeep(v, seen)
    end
    return b
end

local function find(a, x)
    for i, v in pairs(a) do
        if v == x then
            return i
        end
    end
    return nil
end

local function count(a)
    local x = 0
    for i, v in pairs(a) do
        x = x + 1
    end
    return x
end

local warn = warn or function(x)
    --dbgprint("! "..tostring(x).." !")
end

local function lookupify(tb)
    for _, v in pairs(tb) do
        tb[v] = true
    end
    return tb
end

--#endregion

--#region Enums

local WhiteChars = lookupify{' ', '\n', '\t', '\r'}

local EscapeForCharacter = {['\r'] = '\\r', ['\n'] = '\\n', ['\t'] = '\\t', ['"'] = '\\"', ["'"] = "\\'", ['\\'] = '\\'}

local CharacterForEscape = {['r'] = '\r', ['n'] = '\n', ['t'] = '\t', ['"'] = '"', ["'"] = "'", ['\\'] = '\\'}

local AllIdentStartChars = lookupify{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
    'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '_'
}

local AllIdentChars = lookupify{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
    'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
    'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '_',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
}

local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}

local HexDigits = lookupify{
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
    'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f'
}

local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#', '.', ':'}

local EqualSymbols = lookupify{'~', '=', '>', '<'}

local Keywords = lookupify{
    'and', 'break', 'do', 'else', 'elseif',
    'end', 'false', 'for', 'function', 'goto', 'if',
    'in', 'local', 'nil', 'not', 'or', 'repeat',
    'return', 'then', 'true', 'until', 'while',
};

local BlockFollowKeyword = lookupify{'else', 'elseif', 'until', 'end'}

local UnopSet = lookupify{'-', 'not', '#'}

local BinopSet = lookupify{
    '+', '-', '*', '/', '%', '^', '#',
    '..', '.', ':',
    '>', '<', '<=', '>=', '~=', '==',
    'and', 'or'
}

local BinaryPriority = {
    ['+'] = {6, 6};
    ['-'] = {6, 6};
    ['*'] = {7, 7};
    ['/'] = {7, 7};
    ['%'] = {7, 7};
    ['^'] = {10, 9};
    ['..'] = {5, 4};
    ['=='] = {3, 3};
    ['~='] = {3, 3};
    ['>'] = {3, 3};
    ['<'] = {3, 3};
    ['>='] = {3, 3};
    ['<='] = {3, 3};
    ['and'] = {2, 2};
    ['or'] = {1, 1};
};

local UnaryPriority = 8

--#endregion

--#region Lua Parser
function FormatTableInt(tb, atIndent, ignoreFunc)
    if tb.Print then
        return tb.Print()
    end
    atIndent = atIndent or 0
    local useNewlines = (count(tb) > 1)
    local baseIndent = string.rep('    ', atIndent+1)
    local out = "{"..(useNewlines and '\n' or '')
    for k, v in pairs(tb) do
        if type(v) ~= 'function' and not ignoreFunc(k) then
            out = out..(useNewlines and baseIndent or '')
            if type(k) == 'number' then
                --nothing to do
            elseif type(k) == 'string' and k:match("^[A-Za-z_][A-Za-z0-9_]*$") then 
                out = out..k.." = "
            elseif type(k) == 'string' then
                out = out.."[\""..k.."\"] = "
            else
                out = out.."["..tostring(k).."] = "
            end
            if type(v) == 'string' then
                out = out.."\""..v.."\""
            elseif type(v) == 'number' then
                out = out..v
            elseif type(v) == 'table' then
                out = out..FormatTableInt(v, atIndent+(useNewlines and 1 or 0), ignoreFunc)
            else
                out = out..tostring(v)
            end
            if next(tb, k) then
                out = out..","
            end
            if useNewlines then
                out = out..'\n'
            end
        end
    end
    out = out..(useNewlines and string.rep('    ', atIndent) or '').."}"
    return out
end

function FormatTable(tb, ignoreFunc)
    ignoreFunc = ignoreFunc or function() 
        return false 
    end
    return FormatTableInt(tb, 0, ignoreFunc)
end

function CreateLuaTokenStream(text)
    -- Tracking for the current position in the buffer, and
    -- the current line / character we are on.
    local p = 1
    local length = #text

    -- Output buffer for tokens
    local tokenBuffer = {}

    -- Get a character, or '' if at eof
    local function look(n)
        n = p + (n or 0)
        if n <= length then
            return text:sub(n, n)
        else
            return ''
        end
    end
    local function get()
        if p <= length then
            local c = text:sub(p, p)
            p = p + 1
            return c
        else
            return ''
        end
    end

    -- Error
    local olderr = error
    local function error(str)
        local q = 1
        local line = 1
        local char = 1
        while q <= p do
            if text:sub(q, q) == '\n' then
                line = line + 1
                char = 1
            else
                char = char + 1
            end
            q = q + 1
        end
        for _, token in pairs(tokenBuffer) do
            --dbgprint(token.Type.."<"..token.Source..">")
        end
        olderr("file<"..line..":"..char..">: "..str)
    end

    -- Consume a long data with equals count of `eqcount'
    local function longdata(eqcount)
        while true do
            local c = get()
            if c == '' then
                error("Unfinished long string.")
            elseif c == ']' then
                local done = true -- Until contested
                for i = 1, eqcount do
                    if look() == '=' then
                        p = p + 1
                    else
                        done = false
                        break
                    end
                end
                if done and get() == ']' then
                    return
                end
            end
        end
    end

    -- Get the opening part for a long data `[` `=`* `[`
    -- Precondition: The first `[` has been consumed
    -- Return: nil or the equals count
    local function getopen()
        local startp = p
        while look() == '=' do
            p = p + 1
        end
        if look() == '[' then
            p = p + 1
            return p - startp - 1
        else
            p = startp
            return nil
        end
    end

    -- Add token
    local whiteStart = 1
    local tokenStart = 1
    local function token(type)
        local tk = {
            Type = type;
            LeadingWhite = text:sub(whiteStart, tokenStart-1);
            Source = text:sub(tokenStart, p-1);
        }
        table.insert(tokenBuffer, tk)
        whiteStart = p
        tokenStart = p
        return tk
    end

    -- Parse tokens loop
    while true do
        -- Mark the whitespace start
        whiteStart = p

        -- Get the leading whitespace + comments
        while true do
            local c = look()
            if c == '' then
                break
            elseif c == '-' then
                if look(1) == '-' then
                    p = p + 2
                    -- Consume comment body
                    if look() == '[' then
                        p = p + 1
                        local eqcount = getopen()
                        if eqcount then
                            -- Long comment body
                            longdata(eqcount)
                        else
                            -- Normal comment body
                            while true do
                                local c2 = get()
                                if c2 == '' or c2 == '\n' then
                                    break
                                end
                            end
                        end
                    else
                        -- Normal comment body
                        while true do
                            local c2 = get()
                            if c2 == '' or c2 == '\n' then
                                break
                            end
                        end
                    end
                else
                    break
                end
            elseif WhiteChars[c] then
                p = p + 1
            else
                break
            end
        end
        local leadingWhite = text:sub(whiteStart, p-1)

        -- Mark the token start
        tokenStart = p

        -- Switch on token type
        local c1 = get()
        if c1 == '' then
            -- End of file
            token('Eof')
            break
        elseif c1 == '\'' or c1 == '\"' then
            -- String constant
            while true do
                local c2 = get()
                if c2 == '\\' then
                    local c3 = get()
                    local esc = CharacterForEscape[c3] or Digits[c3]
                    if not esc then
                        error("Invalid Escape Sequence `"..c3.."`.")
                    end
                elseif c2 == c1 then
                    break
                end
            end
            token('String')
        elseif AllIdentStartChars[c1] then
            -- Ident or Keyword
            while AllIdentChars[look()] do
                p = p + 1
            end
            if Keywords[text:sub(tokenStart, p-1)] then
                token('Keyword')
            else
                token('Ident')
            end
        elseif Digits[c1] or (c1 == '.' and Digits[look()]) then
            -- Number
            if c1 == '0' and look() == 'x' then
                p = p + 1
                -- Hex number
                while HexDigits[look()] do
                    p = p + 1
                end
            else
                -- Normal Number
                while Digits[look()] do
                    p = p + 1
                end
                if look() == '.' then
                    -- With decimal point
                    p = p + 1
                    while Digits[look()] do
                        p = p + 1
                    end
                end
                if look() == 'e' or look() == 'E' then
                    -- With exponent
                    p = p + 1
                    if look() == '-' then
                        p = p + 1
                    end
                    while Digits[look()] do
                        p = p + 1
                    end
                end
            end
            token('Number')
        elseif c1 == '[' then
            -- '[' Symbol or Long String
            local eqCount = getopen()
            if eqCount then
                -- Long string
                longdata(eqCount)
                token('String')
            else
                -- Symbol
                token('Symbol')
            end
        elseif c1 == '.' then
            -- Greedily consume up to 3 `.` for . / .. / ... tokens
            if look() == '.' then
                get()
                if look() == '.' then
                    get()
                end
            end
            token('Symbol')
        elseif EqualSymbols[c1] then
            if look() == '=' then
                p = p + 1
            end
            token('Symbol')
        elseif Symbols[c1] then
            token('Symbol')
        else
            error("Bad symbol `"..c1.."` in source.")
        end
    end
    return tokenBuffer
end

function CreateLuaParser(text)
    -- Token stream and pointer into it
    local tokens = CreateLuaTokenStream(text)
    -- for _, tok in pairs(tokens) do
    -- 	print(tok.Type..": "..tok.Source)
    -- end
    local p = 1

    local function get()
        local tok = tokens[p]
        if p < #tokens then
            p = p + 1
        end
        return tok
    end
    local function peek(n)
        n = p + (n or 0)
        return tokens[n] or tokens[#tokens]
    end

    local function getTokenStartPosition(token)
        local line = 1
        local char = 0
        local tkNum = 1
        while true do
            local tk = tokens[tkNum]
            local text;
            if tk == token then
                text = tk.LeadingWhite
            else
                text = tk.LeadingWhite..tk.Source
            end
            for i = 1, #text do
                local c = text:sub(i, i)
                if c == '\n' then
                    line = line + 1
                    char = 0
                else
                    char = char + 1
                end
            end
            if tk == token then
                break
            end
            tkNum = tkNum + 1
        end
        return line..":"..(char+1)
    end
    local function debugMark()
        local tk = peek()
        return "<"..tk.Type.." `"..tk.Source.."`> at: "..getTokenStartPosition(tk)
    end

    local function isBlockFollow()
        local tok = peek()
        return tok.Type == 'Eof' or (tok.Type == 'Keyword' and BlockFollowKeyword[tok.Source])
    end	
    local function isUnop()
        return UnopSet[peek().Source] or false
    end
    local function isBinop()
        return BinopSet[peek().Source] or false
    end

    local function expect(typenameorlist, source)
        local result
        local tk = peek()

        local function checkforspecific(typename)
            if tk.Type == typename and (source == nil or tk.Source == source) then
                return get()
            else
                return nil
            end
        end

        local function listdebugtokens()
            for i = -3, 3 do
                --dbgprint((i == 0 and "HERE --> " or "").."Tokens["..i.."] = `"..peek(i).Source.."`: ",peek(i))
            end
        end

        if type(typenameorlist) == "table" then
            for _, typename in pairs(typenameorlist) do
                result = checkforspecific(typename)
                if result then
                    break
                end
            end

            if not result then
                listdebugtokens()
                return error(getTokenStartPosition(tk).."Expected one of types: "..table.concat(typenameorlist, ", ")..".")
            end
        elseif type(typenameorlist) == "string" then
            result = checkforspecific(typenameorlist)

            if not result then
                listdebugtokens()
                return error(getTokenStartPosition(tk).." Expected type: "..typenameorlist..(source and ". as: `"..source.."`" or ""), 0)
            end
        else
            error("expect error, expect arg is not valid", 0)
        end


        return result
    end

    local function MkNode(node)
        local getf = node.GetFirstToken
        local getl = node.GetLastToken
        function node:GetFirstToken()
            local t = getf(self)
            assert(t)
            return t
        end
        function node:GetLastToken()
            local t = getl(self)
            assert(t)
            return t
        end
        return node
    end

    -- Forward decls
    local block;
    local expr;

    -- Expression list
    local function exprlist()
        local exprList = {}
        local commaList = {}
        table.insert(exprList, expr())
        while peek().Source == ',' do
            table.insert(commaList, get())
            table.insert(exprList, expr())
        end
        return exprList, commaList
    end

    local function prefixexpr()
        local tk = peek()
        if tk.Source == '(' then
            local oparenTk = get()
            local inner = expr()
            local cparenTk = expect('Symbol', ')')
            return MkNode{
                Type = 'ParenExpr';
                Expression = inner;
                Token_OpenParen = oparenTk;
                Token_CloseParen = cparenTk;
                GetFirstToken = function(self)
                    return self.Token_OpenParen
                end;
                GetLastToken = function(self)
                    return self.Token_CloseParen
                end;
            }
        elseif tk.Type == 'Ident' then
            return MkNode{
                Type = 'VariableExpr';
                Token = get();
                GetFirstToken = function(self)
                    return self.Token
                end;
                GetLastToken = function(self)
                    return self.Token
                end;
            }
        else
            --dbgprint(debugMark())
            error(getTokenStartPosition(tk)..": Unexpected symbol")
        end
    end

    local function tableexpr()
        local obrace = expect('Symbol', '{')
        local entries = {}
        local separators = {}
        while peek().Source ~= '}' do
            if peek().Source == '[' then
                -- Index
                local obrac = get()
                local index = expr()
                local cbrac = expect('Symbol', ']')
                local eq = expect('Symbol', '=')
                local value = expr()
                table.insert(entries, {
                    EntryType = 'Index';
                    Index = index;
                    Value = value;
                    Token_OpenBracket = obrac;
                    Token_CloseBracket = cbrac;
                    Token_Equals = eq;
                })
            elseif peek().Type == 'Ident' and peek(1).Source == '=' then
                -- Field
                local field = get()
                local eq = get()
                local value = expr()
                table.insert(entries, {
                    EntryType = 'Field';
                    Field = field;
                    Value = value;
                    Token_Equals = eq;
                })
            else
                -- Value
                local value = expr()
                table.insert(entries, {
                    EntryType = 'Value';
                    Value = value;
                })
            end

            -- Comma or Semicolon separator
            if peek().Source == ',' or peek().Source == ';' then
                table.insert(separators, get())
            else
                break
            end
        end
        local cbrace = expect('Symbol', '}')
        return MkNode{
            Type = 'TableLiteral';
            EntryList = entries;
            Token_SeparatorList = separators;
            Token_OpenBrace = obrace;
            Token_CloseBrace = cbrace;
            GetFirstToken = function(self)
                return self.Token_OpenBrace
            end;
            GetLastToken = function(self)
                return self.Token_CloseBrace
            end;
        }
    end

    -- List of identifiers
    local function varlist()
        --dbgprint('varlist')
        local varList = {}
        local commaList = {}
        if peek().Type == 'Ident' or peek().Source == '...' then --only one var
            table.insert(varList, get())
        end
        while peek().Source == ',' do
            table.insert(commaList, get())
            local id = expect({'Ident', "Symbol"})
            --print('listid: ', id)
            table.insert(varList, id)
        end
        return varList, commaList
    end

    -- Body
    local function blockbody(terminator)
        local body = block()
        local after = peek()
        if after.Type == 'Keyword' and after.Source == terminator then
            get()
            return body, after
        else
            --dbgprint(after.Type, after.Source)
            error(getTokenStartPosition(after)..": "..terminator.." expected.")
        end
    end

    -- Function declaration
    local function funcdecl(isAnonymous)
        local functionKw = get()
        --
        local nameChain;
        local nameChainSeparator;
        --
        if not isAnonymous then
            nameChain = {}
            nameChainSeparator = {}
            --
            table.insert(nameChain, expect('Ident'))
            --
            while peek().Source == '.' do
                table.insert(nameChainSeparator, get())
                table.insert(nameChain, expect('Ident'))
            end
            if peek().Source == ':' then
                table.insert(nameChainSeparator, get())
                table.insert(nameChain, expect('Ident'))
            end
        end
        --
        local oparenTk = expect('Symbol', '(')
        local argList, argCommaList = varlist()
        local cparenTk = expect('Symbol', ')')
        local fbody, enTk = blockbody('end')
        --
        return MkNode{
            Type = (isAnonymous and 'FunctionLiteral' or 'FunctionStat');
            NameChain = nameChain;
            ArgList = argList;
            Body = fbody;
            --
            Token_Function = functionKw;
            Token_NameChainSeparator = nameChainSeparator;
            Token_OpenParen = oparenTk;
            Token_ArgCommaList = argCommaList;
            Token_CloseParen = cparenTk;
            Token_End = enTk;
            GetFirstToken = function(self)
                return self.Token_Function
            end;
            GetLastToken = function(self)
                return self.Token_End;
            end;
        }
    end

    -- Argument list passed to a funciton
    local function functionargs()
        local tk = peek()
        if tk.Source == '(' then
            local oparenTk = get()
            local argList = {}
            local argCommaList = {}
            while peek().Source ~= ')' do
                table.insert(argList, expr())
                if peek().Source == ',' then
                    table.insert(argCommaList, get())
                else
                    break
                end
            end
            local cparenTk = expect('Symbol', ')')
            return MkNode{
                CallType = 'ArgCall';
                ArgList = argList;
                --
                Token_CommaList = argCommaList;
                Token_OpenParen = oparenTk;
                Token_CloseParen = cparenTk;
                GetFirstToken = function(self)
                    return self.Token_OpenParen
                end;
                GetLastToken = function(self)
                    return self.Token_CloseParen
                end;
            }
        elseif tk.Source == '{' then
            return MkNode{
                CallType = 'TableCall';
                TableExpr = expr();
                GetFirstToken = function(self)
                    return self.TableExpr:GetFirstToken()
                end;
                GetLastToken = function(self)
                    return self.TableExpr:GetLastToken()
                end;
            }
        elseif tk.Type == 'String' then
            return MkNode{
                CallType = 'StringCall';
                Token = get();
                GetFirstToken = function(self)
                    return self.Token
                end;
                GetLastToken = function(self)
                    return self.Token
                end;
            }
        else
            error("Function arguments expected.")
        end
    end

    local function primaryexpr() --Get an IDENT, also process indexes, calls and other stuff
        local base = prefixexpr() --Get IDENT
        assert(base, "nil prefixexpr")
        while true do
            local tk = peek()
            if tk.Source == '.' then
                local dotTk = get()
                local fieldName = expect('Ident')
                base = MkNode{
                    Type = 'FieldExpr';
                    Base = base;
                    Field = fieldName;
                    Token_Dot = dotTk;
                    GetFirstToken = function(self)
                        return self.Base:GetFirstToken()
                    end;
                    GetLastToken = function(self)
                        return self.Field
                    end;
                }
            elseif tk.Source == ':' then
                local colonTk = get()
                local methodName = expect('Ident')
                local fargs = functionargs()
                base = MkNode{
                    Type = 'MethodExpr';
                    Base = base;
                    Method = methodName;
                    FunctionArguments = fargs;
                    Token_Colon = colonTk;
                    GetFirstToken = function(self)
                        return self.Base:GetFirstToken()
                    end;
                    GetLastToken = function(self)
                        return self.FunctionArguments:GetLastToken()
                    end;
                }
            elseif tk.Source == '[' then
                local obrac = get()
                local index = expr()
                local cbrac = expect('Symbol', ']')
                base = MkNode{
                    Type = 'IndexExpr';
                    Base = base;
                    Index = index;
                    Token_OpenBracket = obrac;
                    Token_CloseBracket = cbrac;
                    GetFirstToken = function(self)
                        return self.Base:GetFirstToken()
                    end;
                    GetLastToken = function(self)
                        return self.Token_CloseBracket
                    end;
                }
            elseif tk.Source == '{' then
                base = MkNode{
                    Type = 'CallExpr';
                    Base = base;
                    FunctionArguments = functionargs();
                    GetFirstToken = function(self)
                        return self.Base:GetFirstToken()
                    end;
                    GetLastToken = function(self)
                        return self.FunctionArguments:GetLastToken()
                    end;
                }
            elseif tk.Source == '(' then
                base = MkNode{
                    Type = 'CallExpr';
                    Base = base;
                    FunctionArguments = functionargs();
                    GetFirstToken = function(self)
                        return self.Base:GetFirstToken()
                    end;
                    GetLastToken = function(self)
                        return self.FunctionArguments:GetLastToken()
                    end;
                }
            else
                return base
            end
        end
    end

    local function simpleexpr()
        local tk = peek()
        if tk.Type == 'Number' then
            return MkNode{
                Type = 'NumberLiteral';
                Token = get();
                GetFirstToken = function(self)
                    return self.Token
                end;
                GetLastToken = function(self)
                    return self.Token
                end;
            }
        elseif tk.Type == 'String' then
            return MkNode{
                Type = 'StringLiteral';
                Token = get();
                GetFirstToken = function(self)
                    return self.Token
                end;
                GetLastToken = function(self)
                    return self.Token
                end;
            }
        elseif tk.Source == 'nil' then
            return MkNode{
                Type = 'NilLiteral';
                Token = get();
                GetFirstToken = function(self)
                    return self.Token
                end;
                GetLastToken = function(self)
                    return self.Token
                end;
            }
        elseif tk.Source == 'true' or tk.Source == 'false' then
            return MkNode{
                Type = 'BooleanLiteral';
                Token = get();
                GetFirstToken = function(self)
                    --dbgprint(self.Token, "is the self token f")
                    return self.Token
                end;
                GetLastToken = function(self)
                    --dbgprint(self.Token, "is the self token l")
                    return self.Token
                end;
            }
        elseif tk.Source == '...' then
            return MkNode{
                Type = 'VargLiteral';
                Token = get();
                GetFirstToken = function(self)
                    return self.Token
                end;
                GetLastToken = function(self)
                    return self.Token
                end;
            }
        elseif tk.Source == '{' then
            return tableexpr()
        elseif tk.Source == 'function' then
            return funcdecl(true)
        else
            return primaryexpr()
        end
    end

    local function subexpr(limit)
        local curNode;

        -- Initial Base Expression
        if isUnop() then
            local opTk = get()
            local ex = subexpr(UnaryPriority)
            curNode = MkNode{
                Type = 'UnopExpr';
                Token_Op = opTk;
                Rhs = ex;
                GetFirstToken = function(self)
                    return self.Token_Op
                end;
                GetLastToken = function(self)
                    return self.Rhs:GetLastToken()
                end;
            }
        else 
            curNode = simpleexpr()
            assert(curNode, "nil simpleexpr")
        end

        -- Apply Precedence Recursion Chain
        while isBinop() and BinaryPriority[peek().Source][1] > limit do
            local opTk = get()
            local rhs = subexpr(BinaryPriority[opTk.Source][2])
            assert(rhs, "RhsNeeded")
            curNode = MkNode{
                Type = 'BinopExpr';
                Lhs = curNode;
                Rhs = rhs;
                Token_Op = opTk;
                GetFirstToken = function(self)
                    --warn("CALLED IT !!!")
                    return self.Lhs:GetFirstToken()
                end;
                GetLastToken = function(self)
                    return self.Rhs:GetLastToken()
                end;
            }
        end

        -- Return result
        return curNode
    end

    -- Expression
    expr = function(limit)
        return subexpr(limit or 0)
    end

    -- Expression statement, a statement where the first token of the statement is a Ident.
    local function exprstat()
        local ex = primaryexpr() --Get the assignment variable, example: `ASSIGNMENTVAR = 123`
        if ex.Type == 'MethodExpr' or ex.Type == 'CallExpr' then --A call expression begins with ident
            -- all good, calls can be statements
            return MkNode{
                Type = 'CallExprStat';
                Expression = ex;
                GetFirstToken = function(self)
                    return self.Expression:GetFirstToken()
                end;
                GetLastToken = function(self)
                    return self.Expression:GetLastToken()
                end;
            }
        else --Assignment expression, begins with ident.
            --Get lhs
            local lhs = {ex}
            local lhsSeparator = {}

            while peek().Source == ',' do
                table.insert(lhsSeparator, get())
                local lhsPart = primaryexpr()
                if lhsPart.Type == 'MethodExpr' or lhsPart.Type == 'CallExpr' then
                    error("Bad left hand side of assignment")
                end
                table.insert(lhs, lhsPart)
            end

            --Get Equals
            local eq = expect('Symbol', '=')
            eq.LeadingWhite = " " --Add some leading white

            --Get Rhs
            local rhs = {expr(0)}
            local rhsSeparator = {}

            ---rhs[1].LeadingWhite = " " --Add some leading white

            while peek().Source == ',' do
                table.insert(rhsSeparator, get())
                table.insert(rhs, expr(1))
            end

            return MkNode{
                Type = 'AssignmentStat';
                Rhs = rhs;
                Lhs = lhs;
                Token_Equals = eq;
                Token_LhsSeparatorList = lhsSeparator;
                Token_RhsSeparatorList = rhsSeparator;
                GetFirstToken = function(self)
                    return self.Lhs[1]:GetFirstToken()
                end;
                GetLastToken = function(self)
                    return self.Rhs[#self.Rhs]:GetLastToken()
                end;
            }
        end
    end

    -- If statement
    local function ifstat()
        local ifKw = get()
        local condition = expr()
        local thenKw = expect('Keyword', 'then')
        local ifBody = block()
        local elseClauses = {}
        while peek().Source == 'elseif' or peek().Source == 'else' do
            local elseifKw = get()
            local elseifCondition, elseifThenKw;
            if elseifKw.Source == 'elseif' then
                elseifCondition = expr()
                elseifThenKw = expect('Keyword', 'then')
            end
            local elseifBody = block()
            table.insert(elseClauses, {
                Condition = elseifCondition;
                Body = elseifBody;
                --
                ClauseType = elseifKw.Source;
                Token = elseifKw;
                Token_Then = elseifThenKw;
            })
            if elseifKw.Source == 'else' then
                break
            end
        end
        local enKw = expect('Keyword', 'end')
        return MkNode{
            Type = 'IfStat';
            Condition = condition;
            Body = ifBody;
            ElseClauseList = elseClauses;
            --
            Token_If = ifKw;
            Token_Then = thenKw;
            Token_End = enKw;
            GetFirstToken = function(self)
                return self.Token_If
            end;
            GetLastToken = function(self)
                return self.Token_End
            end;
        }
    end

    -- Do statement
    local function dostat()
        local doKw = get()
        local body, enKw = blockbody('end')
        --
        return MkNode{
            Type = 'DoStat';
            Body = body;
            --
            Token_Do = doKw;
            Token_End = enKw;
            GetFirstToken = function(self)
                return self.Token_Do
            end;
            GetLastToken = function(self)
                return self.Token_End
            end;
        }
    end

    -- While statement
    local function whilestat()
        local whileKw = get()
        local condition = expr()
        local doKw = expect('Keyword', 'do')
        local body, enKw = blockbody('end')
        --
        return MkNode{
            Type = 'WhileStat';
            Condition = condition;
            Body = body;
            --
            Token_While = whileKw;
            Token_Do = doKw;
            Token_End = enKw;
            GetFirstToken = function(self)
                return self.Token_While
            end;
            GetLastToken = function(self)
                return self.Token_End
            end;
        }
    end

    -- For statement
    local function forstat()
        local forKw = get()
        local loopVars, loopVarCommas = varlist()
        local node = {}
        if peek().Source == '=' then
            local eqTk = get()
            local exprList, exprCommaList = exprlist()
            if #exprList < 2 or #exprList > 3 then
                error("expected 2 or 3 values for range bounds")
            end
            local doTk = expect('Keyword', 'do')
            local body, enTk = blockbody('end')
            return MkNode{
                Type = 'NumericForStat';
                VarList = loopVars;
                RangeList = exprList;
                Body = body;
                --
                Token_For = forKw;
                Token_VarCommaList = loopVarCommas;
                Token_Equals = eqTk;
                Token_RangeCommaList = exprCommaList;
                Token_Do = doTk;
                Token_End = enTk;
                GetFirstToken = function(self)
                    return self.Token_For
                end;
                GetLastToken = function(self)
                    return self.Token_End
                end;
            }
        elseif peek().Source == 'in' then
            local inTk = get()
            local exprList, exprCommaList = exprlist()
            local doTk = expect('Keyword', 'do')
            local body, enTk = blockbody('end')
            return MkNode{
                Type = 'GenericForStat';
                VarList = loopVars;
                GeneratorList = exprList;
                Body = body;
                --
                Token_For = forKw;
                Token_VarCommaList = loopVarCommas;
                Token_In = inTk;
                Token_GeneratorCommaList = exprCommaList;
                Token_Do = doTk;
                Token_End = enTk;
                GetFirstToken = function(self)
                    return self.Token_For
                end;
                GetLastToken = function(self)
                    return self.Token_End
                end;
            }
        else
            error("`=` or in expected")
        end
    end

    -- Repeat statement
    local function repeatstat()
        local repeatKw = get()
        local body, untilTk = blockbody('until')
        local condition = expr()
        return MkNode{
            Type = 'RepeatStat';
            Body = body;
            Condition = condition;
            --
            Token_Repeat = repeatKw;
            Token_Until = untilTk;
            GetFirstToken = function(self)
                return self.Token_Repeat
            end;
            GetLastToken = function(self)
                return self.Condition:GetLastToken()
            end;
        }
    end

    -- Local var declaration
    local function localdecl()
        local localKw = get()
        if peek().Source == 'function' then
            -- Local function def
            local funcStat = funcdecl(false)
            if #funcStat.NameChain > 1 then
                error(getTokenStartPosition(funcStat.Token_NameChainSeparator[1])..": `(` expected.")
            end
            return MkNode{
                Type = 'LocalFunctionStat';
                FunctionStat = funcStat;
                Token_Local = localKw;
                GetFirstToken = function(self)
                    return self.Token_Local
                end;
                GetLastToken = function(self)
                    return self.FunctionStat:GetLastToken()
                end;
            }
        elseif peek().Type == 'Ident' then
            -- Local variable declaration
            local varList, varCommaList = varlist()
            local exprList, exprCommaList = {}, {}
            local eqToken;
            if peek().Source == '=' then
                eqToken = get()
                exprList, exprCommaList = exprlist()
            end
            return MkNode{
                Type = 'LocalVarStat';
                VarList = varList;
                ExprList = exprList;
                Token_Local = localKw;
                Token_Equals = eqToken;
                Token_VarCommaList = varCommaList;
                Token_ExprCommaList = exprCommaList;	
                GetFirstToken = function(self)
                    return self.Token_Local
                end;
                GetLastToken = function(self)
                    if #self.ExprList > 0 then
                        return self.ExprList[#self.ExprList]:GetLastToken()
                    else
                        return self.VarList[#self.VarList]
                    end
                end;
            }
        else
            error("`function` or ident expected")
        end
    end

    -- Return statement
    local function retstat()
        local returnKw = get()
        local exprList;
        local commaList;
        if isBlockFollow() or peek().Source == ';' then
            exprList = {}
            commaList = {}
        else
            exprList, commaList = exprlist()
        end
        return {
            Type = 'ReturnStat';
            ExprList = exprList;
            Token_Return = returnKw;
            Token_CommaList = commaList;
            GetFirstToken = function(self)
                return self.Token_Return
            end;
            GetLastToken = function(self)
                if #self.ExprList > 0 then
                    return self.ExprList[#self.ExprList]:GetLastToken()
                else
                    return self.Token_Return
                end
            end;
        }
    end

    -- Break statement
    local function breakstat()
        local breakKw = get()
        return {
            Type = 'BreakStat';
            Token_Break = breakKw;
            GetFirstToken = function(self)
                return self.Token_Break
            end;
            GetLastToken = function(self)
                return self.Token_Break
            end;
        }
    end

    -- Expression
    local function statement()
        local tok = peek()
        if tok.Source == 'if' then
            return false, ifstat()
        elseif tok.Source == 'while' then
            return false, whilestat()
        elseif tok.Source == 'do' then
            return false, dostat()
        elseif tok.Source == 'for' then
            return false, forstat()
        elseif tok.Source == 'repeat' then
            return false, repeatstat()
        elseif tok.Source == 'function' then
            return false, funcdecl(false)
        elseif tok.Source == 'local' then
            return false, localdecl()
        elseif tok.Source == 'return' then
            return true, retstat()
        elseif tok.Source == 'break' then
            return true, breakstat()
        else
            return false, exprstat()
        end
    end

    -- Chunk
    block = function()
        local statements = {}
        local semicolons = {}
        local isLast = false
        while not isLast and not isBlockFollow() do
            -- Parse statement
            local stat;
            isLast, stat = statement()
            table.insert(statements, stat)
            local next = peek()
            if next.Type == 'Symbol' and next.Source == ';' then
                semicolons[#statements] = get()
            end
        end
        return {
            Type = 'StatList';
            StatementList = statements;
            SemicolonList = semicolons;
            GetFirstToken = function(self)
                if #self.StatementList == 0 then
                    return nil
                else
                    return self.StatementList[1]:GetFirstToken()
                end
            end;
            GetLastToken = function(self)
                if #self.StatementList == 0 then
                    return nil
                elseif self.SemicolonList[#self.StatementList] then
                    -- Last token may be one of the semicolon separators
                    return self.SemicolonList[#self.StatementList]
                else
                    return self.StatementList[#self.StatementList]:GetLastToken()
                end
            end
        }
    end

    return block()
end
--#endregion

--#region AST Modification

function VisitAst(ast, visitors)
    local ExprType = lookupify{
        'BinopExpr'; 'UnopExpr'; 
        'NumberLiteral'; 'StringLiteral'; 'NilLiteral'; 'BooleanLiteral'; 'VargLiteral';
        'FieldExpr'; 'IndexExpr';
        'MethodExpr'; 'CallExpr';
        'FunctionLiteral';
        'VariableExpr';
        'ParenExpr';
        'TableLiteral';
    }

    local StatType = lookupify{
        'StatList';
        'BreakStat';
        'ReturnStat';
        'LocalVarStat';
        'LocalFunctionStat';
        'FunctionStat';
        'RepeatStat';
        'GenericForStat';
        'NumericForStat';
        'WhileStat';
        'DoStat';
        'IfStat';
        'CallExprStat';
        'AssignmentStat';
    }

    -- Check for typos in visitor construction
    for visitorSubject, visitor in pairs(visitors) do
        if not StatType[visitorSubject] and not ExprType[visitorSubject] then
            error("Invalid visitor target: `"..visitorSubject.."`")
        end
    end

    -- Helpers to call visitors on a node
    local function preVisit(exprOrStat, list)
        local visitor = visitors[exprOrStat.Type]
        if type(visitor) == 'function' then
            return visitor(exprOrStat, list)
        elseif visitor and visitor.Pre then
            return visitor.Pre(exprOrStat, list)
        end
    end
    local function postVisit(exprOrStat, list)
        local visitor = visitors[exprOrStat.Type]
        if visitor and type(visitor) == 'table' and visitor.Post then
            return visitor.Post(exprOrStat, list)
        end
    end

    local visitExpr, visitStat;

    visitExpr = function(expr)
        if preVisit(expr) then
            -- Handler did custom child iteration or blocked child iteration
            return
        end
        if expr.Type == 'BinopExpr' then
            visitExpr(expr.Lhs)
            visitExpr(expr.Rhs)
        elseif expr.Type == 'UnopExpr' then
            visitExpr(expr.Rhs)
        elseif expr.Type == 'NumberLiteral' or expr.Type == 'StringLiteral' or 
            expr.Type == 'NilLiteral' or expr.Type == 'BooleanLiteral' or 
            expr.Type == 'VargLiteral' 
        then
            -- No children to visit, single token literals
        elseif expr.Type == 'FieldExpr' then
            visitExpr(expr.Base)
        elseif expr.Type == 'IndexExpr' then
            visitExpr(expr.Base)
            visitExpr(expr.Index)
        elseif expr.Type == 'MethodExpr' or expr.Type == 'CallExpr' then
            visitExpr(expr.Base)
            if expr.FunctionArguments.CallType == 'ArgCall' then
                for index, argExpr in ipairs(expr.FunctionArguments.ArgList) do
                    visitExpr(argExpr)
                end
            elseif expr.FunctionArguments.CallType == 'TableCall' then
                visitExpr(expr.FunctionArguments.TableExpr)
            end
        elseif expr.Type == 'FunctionLiteral' then
            visitStat(expr.Body)
        elseif expr.Type == 'VariableExpr' then
            -- No children to visit
        elseif expr.Type == 'ParenExpr' then
            visitExpr(expr.Expression)
        elseif expr.Type == 'TableLiteral' then
            for index, entry in ipairs(expr.EntryList) do
                if entry.EntryType == 'Field' then
                    visitExpr(entry.Value)
                elseif entry.EntryType == 'Index' then
                    visitExpr(entry.Index)
                    visitExpr(entry.Value)
                elseif entry.EntryType == 'Value' then
                    visitExpr(entry.Value)
                else
                    error("unreachable")
                end
            end
        else
            error("unreachable, type: "..expr.Type..":"..FormatTable(expr))
        end
        postVisit(expr)
    end

    visitStat = function(stat, list)
        if preVisit(stat, list) then
            -- Handler did custom child iteration or blocked child iteration
            return
        end
        if stat.Type == 'StatList' then
            for index, ch in ipairs(stat.StatementList) do
                if list and list.Type == "Statlist" then
                    error()
                end
                visitStat(ch, list)
            end
        elseif stat.Type == 'BreakStat' then
            -- No children to visit
        elseif stat.Type == 'ReturnStat' then
            for index, expr in ipairs(stat.ExprList) do
                visitExpr(expr)
            end
        elseif stat.Type == 'LocalVarStat' then
            if stat.Token_Equals then
                for index, expr in ipairs(stat.ExprList) do
                    visitExpr(expr)
                end
            end
        elseif stat.Type == 'LocalFunctionStat' then
            visitStat(stat.FunctionStat.Body, stat)
        elseif stat.Type == 'FunctionStat' then
            visitStat(stat.Body, stat)
        elseif stat.Type == 'RepeatStat' then
            visitStat(stat.Body, stat)
            visitExpr(stat.Condition)
        elseif stat.Type == 'GenericForStat' then
            for index, expr in ipairs(stat.GeneratorList) do
                visitExpr(expr)
            end
            visitStat(stat.Body, stat)
        elseif stat.Type == 'NumericForStat' then
            for index, expr in ipairs(stat.RangeList) do
                visitExpr(expr)
            end
            visitStat(stat.Body, stat)
        elseif stat.Type == 'WhileStat' then
            visitExpr(stat.Condition)
            visitStat(stat.Body, stat)
        elseif stat.Type == 'DoStat' then
            visitStat(stat.Body, stat)
        elseif stat.Type == 'IfStat' then
            visitExpr(stat.Condition)
            visitStat(stat.Body, stat)
            for _, clause in ipairs(stat.ElseClauseList) do
                if clause.Condition then
                    visitExpr(clause.Condition)
                end
                visitStat(clause.Body)
            end
        elseif stat.Type == 'CallExprStat' then
            visitExpr(stat.Expression)
        elseif stat.Type == 'AssignmentStat' then
            for index, ex in ipairs(stat.Lhs) do
                visitExpr(ex)
            end
            for index, ex in ipairs(stat.Rhs) do
                visitExpr(ex)
            end
        else
            error("unreachable")
        end	
        postVisit(stat, list)
    end

    if StatType[ast.Type] then
        visitStat(ast)
    else
        visitExpr(ast)
    end
end

-- Adds Info for every variable, via VisitAst
function AddVariableInfo(ast)
    -- Numbering generator for variable lifetimes
    local locationGenerator = 0
    local function markLocation()
        locationGenerator = locationGenerator + 1
        return locationGenerator
    end

    local globalVars = {}
    local rootscope = {
        BeginLocation = 0;
        ChildScopeList = {};
        Depth = 0;
        VariableList = {};
        GetVar = (function(x) end);
    }

    local currentScope = rootscope

    -- Scope management
    local function pushScope(Function, FuncVar, Stat, Loop)
        local previousscope = currentScope
        currentScope = {
            ParentScope = currentScope;
            ChildScopeList = {};
            UsedVars = {};
            Statement = Stat or Function;
            VariableList = {};
            BeginLocation = markLocation();
        }
		--[[
		purpose of UsedVars:
		 so we know not to predict constants if this functions scope is ever called
		 example:
			```lua
			local lol = 1
			local function wow(arb)
					lol =  12
			end

			lol = 43
			wow()
			print(lol + 1)
			```
		 it assumes that lol is 43 so it simplifies `print(lol + 1)` into `print( 44)`
		 but because we are calling `wow` and changing `lol` after the assignment,
		 it would actually be 12 when we add 1.
		 with this feature we add a true on the stack insigniating that the call expression
		 effects this variable, so we are no longer certian of its value and should not simplify
		]]
        currentScope.Function = Function or (previousscope and previousscope.Function);
        currentScope.Loop = Loop;
        if Stat then
            Stat.Scope = currentScope
        elseif Function then
            Function.Scope = currentScope
        end
        if FuncVar then -- true 
            --dbgprinttab(FuncVar)
            if not FuncVar.Calls then
                FuncVar.Calls = {}
            end
            FuncVar.FunctionScope = currentScope
        end
        --previousscope = nil
        if currentScope.ParentScope then
            currentScope.Depth = currentScope.ParentScope.Depth + 1
            table.insert(currentScope.ParentScope.ChildScopeList, currentScope)
        else
            currentScope.Depth = 1
        end
        function currentScope:GetVar(varName)
            for _, var in pairs(self.VariableList) do
                if var.Name == varName then
                    return var
                end
            end
            --upvalues and other scopes
            if self.ParentScope then
                return self.ParentScope:GetVar(varName)
            else
                for _, var in pairs(globalVars) do
                    if var.Name == varName then
                        return var
                    end
                end
            end
        end
    end
    local function popScope()
        local scope = currentScope

        -- Mark where this scope ends
        scope.EndLocation = markLocation()

        -- Mark all of the variables in the scope as ending there
        for _, var in pairs(scope.VariableList) do
            var.ScopeEndLocation = scope.EndLocation
        end

        -- Move to the parent scope
        currentScope = scope.ParentScope

        return scope
    end
    pushScope({Type = "root", root = true}) -- push initial scope

    -- Add / reference variables.
    local function addLocalVar(name, setNameFunc, localInfo, LiteralVal)
        assert(localInfo, "Misisng localInfo")
        assert(name, "Missing local var name")
        local startlocation = markLocation()
        --dbgprint("edt45g")
        --dbgprinttab(LiteralVal,1)
        local var = {
            Type = 'Local';
            Name = name;
            RenameList = {setNameFunc};
            AssignedTo = false;
            Info2 = localInfo;
            UseCount = 0;
            LiteralStack = {{LiteralVal, currentScope}}; --LiteralVal = &LiteralVal,
            Scope = currentScope;
            BeginLocation = startlocation;
            EndLocation = startlocation;
            ReferenceLocationList = {startlocation};
        }

        function var:AddType(type)
            --Attempt to infer variables type, based of references (e.g. a boolean)
            table.insert(self.UsedTypes, type)
        end

        function var:Rename(newName)
            self.Name = newName
            for _, renameFunc in pairs(self.RenameList) do
                renameFunc(newName)
            end
        end
        function var:Reference()
            self.UseCount = self.UseCount + 1
        end
        table.insert(currentScope.VariableList, var)
        return var
    end
    local function getGlobalVar(name)
        for _, var in pairs(globalVars) do
            if var.Name == name then
                return var
            end
        end
        --if not found then try to make a global var then return that
        local var = {
            Type = 'Global';
            Name = name;
            RenameList = {};
            LiteralStack = {}; --LiteralVal = &LiteralVal,
            AssignedTo = false;
            UseCount = 0;
            Scope = nil; -- Globals have no scope
            BeginLocation = markLocation();
            EndLocation = markLocation();
            ReferenceLocationList = {};
        }
        function var:Rename(newName)
            self.Name = newName
            for _, renameFunc in pairs(self.RenameList) do
                renameFunc(newName)
            end
        end
        function var:Reference()
            self.UseCount = self.UseCount + 1
        end
        table.insert(globalVars, var)
        return var
    end
    local function addGlobalReference(name, setNameFunc)
        assert(name, "Missing var name")
        local var = getGlobalVar(name)
        table.insert(var.RenameList, setNameFunc)
        local var2 = {
            Info = var;
            Location = markLocation();
            Scope = currentScope;
        }
        return var2
    end
    local function getLocalVar(scope, name)
        -- First search this scope
        -- Note: Reverse iterate here because Lua does allow shadowing a local
        --       within the same scope, and the later defined variable should
        --       be the one referenced.
        for i = #scope.VariableList, 1, -1 do
            if scope.VariableList[i].Name == name then
                return scope.VariableList[i]
            end
        end

        -- Then search parent scope
        if scope.ParentScope then
            local var = getLocalVar(scope.ParentScope, name)
            if var then
                return var
            end
        end

        -- Then 
        return nil
    end
    --Checks for var, if exists, is reference, else is a global.
    local function referenceVariable(name, setNameFunc)
        assert(name, "Missing var name")
        local var = getLocalVar(currentScope, name)
        if var then
            table.insert(var.RenameList, setNameFunc)
        else
            return addGlobalReference(name, setNameFunc)
        end
        -- Update the end location of where this variable is used, and
        -- add this location to the list of references to this variable.
        local curLocation = markLocation()
        var.EndLocation = curLocation
        --dbgprinttab(var)
        table.insert(var.ReferenceLocationList, var.EndLocation)
        table.insert(currentScope.UsedVars, var)
        local var2 = {
            Info = var;
            Location = curLocation;
            Scope = currentScope;
        }
        return var2
    end

    local function updateusedvarscalls()

    end

    local visitor = {}
    visitor.FunctionLiteral = {
        -- Function literal adds a new scope and adds the function literal arguments
        -- as local variables in the scope.
        Pre = function(expr)
            pushScope(expr)
            for index, ident in pairs(expr.ArgList) do --variables created by arguments like: `function myfunc(a, b)` being `a` and `b`
                local var = addLocalVar(ident.Source, function(name)
                    ident.Source = name
                end, {
                    Type = 'Argument';
                    Index = index;
                }, nil)
            end
        end;
        Post = function(expr)
            popScope()
        end;
    }
    visitor.VariableExpr = function(expr)
        -- Variable expression references from existing local varibales
        -- in the current scope, annotating the variable usage with variable
        -- information.
        expr.Variable = referenceVariable(expr.Token.Source, function(newName)
            expr.Token.Source = newName
        end)
    end
    visitor.StatList = {
        -- StatList adds a new scope
        Pre = function(statlist, mainstat)
            pushScope(nil, nil, mainstat)
        end;
        Post = function(statlist, mainstat)
            popScope()
        end;
    }
    visitor.LocalVarStat = {
        Post = function(stat)
            -- Local var stat adds the local variables to the current scope as locals
            -- We need to visit the subexpressions first, because these new locals
            -- will not be in scope for the initialization value expressions. That is:
            --  `local bar = bar + 1`
            -- Is valid code
            for varNum, ident in pairs(stat.VarList) do
                local expr = stat.ExprList[varNum]
                addLocalVar(ident.Source, function(name)
                    stat.VarList[varNum].Source = name
                end, {
                    Type = 'Local';
                }, expr)
            end		
        end;
    }
    visitor.LocalFunctionStat = {
        Pre = function(stat)
            -- Local function stat adds the function itself to the current scope as
            -- a local variable, and creates a new scope with the function arguments
            -- as local variables.
            local var = addLocalVar(stat.FunctionStat.NameChain[1].Source, function(name)
                stat.FunctionStat.NameChain[1].Source = name
            end, {
                Type = 'LocalFunction';
            }, nil)
            pushScope(stat, var)
            for index, ident in pairs(stat.FunctionStat.ArgList) do
                addLocalVar(ident.Source, function(name)
                    ident.Source = name
                end, {
                    Type = 'Argument';
                    Index = index;
                }, nil)
            end
        end;
        Post = function()
            popScope()
        end;
    }
    visitor.CallExprStat = {
        Post = function(stat)
            local ex = stat.Expression.Base
            local var = ex.Variable
            if var and var.Info and ex.Token and ex.Token.Source then
                --dbgprint("call expr thing!")
                --dbgprinttab(stat)
                if not var.Info.Calls then
                    var.Info.Calls = {{stat, currentScope.Function.Scope.UsedVars}}
                else
                    table.insert(var.Info.Calls, {stat, currentScope.Function.Scope.UsedVars})
                end
                --dbgprint("functions info:")
                --dbgprinttab(var.Info)
                local scope = var.Info.FunctionScope
                if scope and scope.UsedVars then
                    local curlocation = ex.Variable.Location
                    for i, var in ipairs(scope.UsedVars) do
                        if var and var.Info then
                            table.insert(currentScope.Function.Scope.UsedVars, var) --this also means we use the var too
                            var.Info.LiteralStack[curlocation] = {stat, currentScope}
                        end
                    end
                end
            end
        end
    }
    visitor.WhileStat = {
        Pre = function (stat)
            pushScope(nil, nil, stat, stat)
        end,
        Post = function (stat)
            popScope()
        end,
    }
    visitor.RepeatStat = {
        Pre = function (stat)
            pushScope(nil, nil, stat, stat)
        end,
        Post = function (stat)
            popScope()
        end,
    }
    visitor.FunctionStat = {
        Pre = function(stat) 			
            -- Function stat adds a new scope containing the function arguments
            -- as local variables.
            -- A function stat may also assign to a global variable if it is in
            -- the form `function foo()` with no additional dots/colons in the 
            -- name chain.
            local nameChain = stat.NameChain
            --dbgprint("the namechain is: ", nameChain, "oh and the stat is: ", stat)
            local var = referenceVariable(nameChain[1].Source, function(name)
                nameChain[1].Source = name
            end)
            if var.Info then
                var.Info.AssignedTo = true
            end
            pushScope(stat, var.Info)
            --dbgprint("ok we have set it")
            --dbgprinttab(var)
            for index, ident in pairs(stat.ArgList) do
                addLocalVar(ident.Source, function(name)
                    ident.Source = name
                end, {
                    Type = 'Argument';
                    Index = index;
                }, nil)
            end
        end;
        Post = function(stat)
            local varfunc = stat.Scope.Statement
            assert(varfunc, "no var func")
            --dbgprint("calls!")
            if varfunc.Info and varfunc.Info.FunctionScope then
                local funcusedvars = varfunc.Info.FunctionScope.UsedVars --if you get an indexing error on this just apply some checks (pls work)
                for _, data in ipairs(varfunc.Info.Calls) do
                    --data: {callstat, callusedvars}
                    local callusedvars = data[2]
                    --dbgprint("call used vars")
                    --dbgprinttab(callusedvars, 2)
                    for _, usedvar in pairs(funcusedvars) do
                        --dbgprint("adding:", usedvar)
                        table.insert(callusedvars, usedvar)
                    end
                end
                varfunc = nil
            end
            popScope()
        end;
    }
    visitor.GenericForStat = {
        Pre = function(stat)
            -- Generic fors need an extra scope holding the range variables
            -- Need a custom visitor so that the generator expressions can be
            -- visited before we push a scope, but the body can be visited
            -- after we push a scope.
            for _, ex in pairs(stat.GeneratorList) do
                VisitAst(ex, visitor)
            end
            pushScope(nil,nil,stat,stat)
            for index, ident in pairs(stat.VarList) do
                --dbgprint("generic for stat: varlist in the for stat idents be like: ", ident)
                addLocalVar(ident.Source, function(name)
                    ident.Source = name
                end, {
                    Type = 'ForRange';
                    Index = index;
                }, nil)
            end
            VisitAst(stat.Body, visitor)
            popScope()
            return true -- Custom visit
        end;
    }
    visitor.NumericForStat = {
        Pre = function(stat)
            -- Numeric fors need an extra scope holding the range variables
            -- Need a custom visitor so that the generator expressions can be
            -- visited before we push a scope, but the body can be visited
            -- after we push a scope.
            for _, ex in pairs(stat.RangeList) do
                VisitAst(ex, visitor)
            end
            pushScope(nil,nil,stat,stat)
            --dbgprint("numeric for stat")
            --dbgprinttab(stat)
            for index, ident in pairs(stat.VarList) do
                addLocalVar(ident.Source, function(name)
                    ident.Source = name
                end, {
                    Type = 'ForRange';
                    Index = index;
                }, nil) --stat.RangeList[1])
            end
            VisitAst(stat.Body, visitor)
            popScope()
            return true	-- Custom visit
        end;
    }
    visitor.AssignmentStat = {
        Post = function(stat)
            -- For an assignment statement we need to mark the
            -- "assigned to" flag on variables.
            for i, ex in pairs(stat.Lhs) do
                if ex.Variable then
                    table.insert(currentScope.Function.Scope.UsedVars, ex.Variable)
                    local curlocation = ex.Variable.Location
                    ex.Variable.Info.AssignedTo = true
                    local rhsval = stat.Rhs[i]
                    --if rhsval.Type ~= "BinopExpr" then
                    ex.Variable.Info.LiteralStack[curlocation] = {rhsval, currentScope}
                    --end
                    --ex.Variable.LiteralVal = stat.Rhs[i]
                end
            end
        end;
    }
	--[[
	visitor.ReturnStat = {
		Post = function(stat)
			local AncestorFunction = currentScope.Function
			if AncestorFunction then
				local returntypes
				for i, expr in ipairs(stat.ExprList) do
					if expr then
						if expr.Type == "VariableExpr" then
							returntypes[i] = expr.Type
						else
							returntypes[i] = expr.Type
						end
					end
				end
				if not AncestorFunction.ReturnTypes then
					AncestorFunction.ReturnTypes = {}
				end
				table.insert(AncestorFunction.ReturnTypes, returntypes)
				print("returned :", stat, "and our function var is:", AncestorFunction)
			end
		end;
	}
	]]

    VisitAst(ast, visitor)

    return globalVars, popScope()
end

--Solve the Solveable math in an ast, Also some other special functions like solveconstants, solveifstats, replaceconstants, solveindexes, etc...
local function SolveMath(ast, solveconstants, solveifstats, replaceconstants, solveindexes)
    local ast = ast
    local canSolve = {
        NumberLiteral = true,
        BooleanLiteral = true,
        StringLiteral = true,
        HashLiteral = true,
        NilLiteral = true,
        TableLiteral = true,
        ParenExpr = true,
        BinopExpr = true
    }

    local replaceTypes = {
        NumberLiteral = true,
        BooleanLiteral = true,
        StringLiteral = true,
        HashLiteral = true,
        NilLiteral = true,
    }

    local function isfinite(num)
        return num == num and num ~= math.huge and num ~= -math.huge
    end

    local function solvebuiltincall()

    end


    local function createtype(type, val, type2, noleadingwhite)
        type2 = type2 or "Number"
        return {
            Type = type,
            Token = {
                Type = type2,
                LeadingWhite = (noleadingwhite and "" or " "),
                Source = val
            },
            GetFirstToken = function(self)
                return self.Token --(noleadingwhite and tostring(val):sub(1,1) or " ")
            end,
            GetLastToken = function(self)
                return self.Token
            end
        }
    end


    local function createbinop(operator, lhs, rhs, leadingwhite)
        return {
            Type = "BinopExpr",
            Token_Op = { Type = "Symbol", LeadingWhite = leadingwhite, Source = operator },
            Lhs = lhs,
            Rhs = rhs,
            GetFirstToken = function()
                return lhs.GetFirstToken()
            end,
            GetLastToken = function()
                return rhs.GetLastToken()
            end
        }
    end

    local function createunop(operator, rhs, leadingwhite)
        return {
            Type = "UnopExpr",
            Token_Op = { Type = "Symbol", LeadingWhite = leadingwhite, Source = operator },
            Rhs = rhs,
            GetFirstToken = function()
                return rhs.Token_Op
            end,
            GetLastToken = function()
                return rhs.GetLastToken()
            end
        }
    end

    --replace without making a new variable, as tables are all "references", replace all of b into a
    local function replace(a, b)
        if b == nil then return end
        for i, v in pairs(b) do
            a[i] = v
        end
    end

    local function removething(a)
        if a == nil or type(a) ~= "string" then return end

        local start = a:sub(1, 1)
        local ret
        if start == '"' or start == "'" then
            ret = a:sub(2, #a - 1)
        elseif start == '[' then
            local count = 0
            local p = 2
            while a:sub(p, p) == '=' do
                count = count + 1
                p = p + 1
            end

            ret = a:sub(2 + count, #a - 2 - count)
        end

        if ret == nil then return '' end

        local newret = ''
        for i = 1, #ret do
            local c = ret:sub(i, i)

            if c == "'" or c == '"' then
                newret = newret .. '\\' .. c
            else
                newret = newret .. c
            end
        end
        return newret
    end

    local function removeParen(a)
        if type(a) == "table" and a.Type == "ParenExpr" then
            a = a.Expression
        end
    end

    --determines if a expression is safe to replace if the stack is no more than 1
    local function safetoreplace(literalstack)  --solving point!!
        for _, literal in pairs(literalstack) do
            if type(literal) == "table" and not replaceTypes[literal.Type] then
                return false
            end
        end
        return true
    end

    local function getfunctionscope(scope)
        if scope.Function then
            return scope.Function
        elseif scope.ParentScope then
            return getfunctionscope(scope.ParentScope)
        end
    end

    local function getscopeloop(scope)
        if scope.Loop then
            return scope.Loop
        elseif scope.ParentScope then
            return getscopeloop(scope.ParentScope)
        end
    end

    --attempt to resolve the literal from a variable
    local function resolveliteral(expr, noreplace)
        local var = expr.Variable

        if solveconstants and var and var.Info then --Make sure it really is a variable

            --dbgprint("literal", var.Location, var.Info.LiteralStack)
            ----dbgprinttab(var.Info.LiteralStack) --Its laggy to print stack...

            dbgprint("stack:")
            dbgprinttab(var.Info.LiteralStack, 2)

            local location = var.Location
            local literalfound, literalscope
            while true do
                if (var.Location - 1) ~= location and var.Location ~= location then --dont want to look for ourselves, as an assignment
                    local data = var.Info.LiteralStack[location]
                    if data then
                        local literaltest, literalscope = data[1], data[2]

                        dbgprint("literal testing")
                        dbgprinttab(literaltest, 2)

                        if literaltest then
                            --dbgprint("testing literal...")
                            --dbgprinttab(literaltest, 1)

                            -- Perform some checks on the literal first..
                            if literaltest.Type == "CallExprStat" or literaltest.Type == "CallExpr" then
                                return --Calls immedately void, as they are impredictable on what they do to the variable, atleast for right now
                            elseif getfunctionscope(literalscope) ~= getfunctionscope(var.Scope) then
                                -- Continue as the function's control flow is unpredictable
                            elseif getscopeloop(literalscope) ~= getscopeloop(var.Scope) then --solving point!, just search if a child instead, but may be laggy... 
                                -- Continue as the function's control flow is unpredictable
                                return
                            elseif not literalscope then
                                --Not good..
                            elseif literalscope.Depth > var.Scope.Depth then
                                -- Lower scopes shouldn't effect us
                            elseif literalscope.Depth == var.Scope.Depth and literalscope ~= var.Scope then 
                                --"You think the same depth correlates us. You are in that scope. I am in this scope. We are not the same."
                            else
                                literalfound = literaltest --This is a good literal
                                break
                            end  
                        end                        
                    elseif location <= 0 then --Never was assigned a literal
                        --dbgprint("not found")
                        break
                    end
                end
                location = location - 1 --Step down
            end

            --dbgprint("found the literal:")
            --dbgprinttab(literalfound, 2)

            if literalfound then --not finding a literal also returns, and if we dont have it in replacetypes. expection that its a global as it might be a built-in
                if literalfound.Type == "VariableExpr" then
                    return resolveliteral(literalfound, noreplace)
                else
                    --dbgprint("the stack:")
                    --dbgprinttab(var.Info.LiteralStack)
                    if replaceconstants and safetoreplace(var.Info.LiteralStack) and not noreplace then --solving point!!
                        replace(expr, literalfound)
                    end
                    return literalfound
                end
            elseif expr.Variable and expr.Variable.Info.Type == "Global" then
                --dbgprint("expection as its global")
                return expr --might just be a built in function
            end
        end

    end

    local function solvebinop(operator, left1, right1, leadingwhite)
        dbgprint("MATHSOLVE: SOLVING BINOP: ",operator,left1,right1)

        local lhs = left1
        local rhs = right1

        --Ignore Parentheses
        if type(left1) == "table" and left1.Type == "ParenExpr" then
            lhs = left1.Expression
        end
        if type(right1) == "table" and right1.Type == "ParenExpr" then
            rhs = right1.Expression
        end


        do --Unknown variables solving
            if lhs.Type == "VariableExpr" then
                lhs = resolveliteral(lhs)
            end

            if rhs.Type == "VariableExpr" then
                rhs = resolveliteral(rhs)
            end

            dbgprint("Binop solve got:", lhs, rhs)
        end

        do --Voids, Checks on if we shouldn't continue
            if
                lhs == nil or rhs == nil --Must exist
                or lhs.Type == nil or rhs.Type == nil
            then
                return
            end

            --Returns could vary
            if lhs.Type == "CallExpr" or rhs.Type == "CallExpr" then
                return
            end

            --Always solve lower binops first!
            if lhs.Type == "BinopExpr" or rhs.Type == "BinopExpr" then
                return
            end

            --We still have variables even after solving..
            if lhs.Type == "VariableExpr" or rhs.Type == "VariableExpr" then
                return
            end
        end

        --Actual solving below here

        local l = (lhs.Token or (lhs.Expression and lhs.Expression.Token)) or nil
        local r = (rhs.Token or (rhs.Expression and rhs.Expression.Token)) or nil

        local lSrc = l and l.Source or nil
        local rSrc = r and r.Source or nil

        local left, right

        if lhs.Type == "BooleanLiteral" then left = lSrc == "true" and true or false end
        if rhs.Type == "BooleanLiteral" then right = rSrc == "true" and true or false end

        if lhs.Type == "NumberLiteral" then
            left = tonumber(lSrc)
            if left == nil then return end
        end
        if rhs.Type == "NumberLiteral" then
            right = tonumber(rSrc)
            if right == nil then return end
        end

        if lhs.Type == "StringLiteral" or lhs.Type == 'HashLiteral' then left = tostring(lSrc) end
        if rhs.Type == "StringLiteral" or rhs.Type == 'HashLiteral' then right = tostring(rSrc) end

        if left ~= nil and right ~= nil then
            if operator == "==" then return left == right end
            if operator == "~=" then return left ~= right end
            if operator == "and" then return left and right end
            if operator == "or" then return left or right end
            if operator == ".." and lhs.Type == "StringLiteral" and rhs.Type == "StringLiteral" then
                return '"' .. removething(lSrc) .. removething(rSrc) .. '"'
            end

            if lhs.Type == "StringLiteral" then left = tonumber(removething(left)) end
            if rhs.Type == "StringLiteral" then right = tonumber(removething(right)) end

            if left == nil or right == nil then return end

            local val
            if operator == "+" then val = left + right end
            if operator == "-" then val = left - right end
            if operator == "*" then val = left * right end
            if operator == "/" then val = left / right end
            if operator == "^" then val = left ^ right end
            if operator == "%" then val = left % right end

            if operator == ">" then val = left > right end
            if operator == "<" then val = left < right end
            if operator == ">=" then val = left >= right end
            if operator == "<=" then val = left <= right end

            if type(val) == "boolean" or (type(val) == "number" and isfinite(val) and val > -(10 ^ 52) and val < 10 ^ 52) then
                return val
            end
        end
    end

    local function solveunop(operator, rhs, leadingwhite)
        local b = rhs.Token or (rhs.Expression and rhs.Expression.Token) or rhs.EntryList or rhs

        if b == nil then return end
        if b.Source == nil and rhs.Type ~= "TableLiteral" then return end

        if rhs.Type == "VariableExpr" or rhs.Type == "CallExpr" or rhs.Type == "BinopExpr" then return end

        local rSrc = b.Source
        local right

        if rhs.Type == "TableLiteral" and b ~= nil then
            local extra = {}
            local amount = 0
            local ignoreRest = false
            local no = false
            local lastIndex = 0

            for i, v in ipairs(b) do
                if ignoreRest then
                    table.insert(extra, v)
                else
                    if v.EntryType == "Value" or v.EntryType == "Index" then
                        if (v.Index == nil or v.Index.Type == "NumberLiteral") and v.Value then
                            local index = (v.Index ~= nil and v.Index.Token ~= nil and v.Index.Token.Source ~= nil) and v.Index.Token.Source or lastIndex + 1

                            if tostring(index) ~= tostring(lastIndex + 1) then
                                ignoreRest = true
                                no = true
                                table.insert(extra, v)
                                break
                            end

                            if v.Value.Type ~= "CallExpr" then
                                amount = amount + 1
                            else
                                ignoreRest = true
                                table.insert(extra, v)
                            end
                        else
                            table.insert(extra, v)
                        end
                    end
                end
            end

            if no then return end

            if operator == "#" then
                rhs.EntryList = extra

                if #rhs.EntryList <= 0 then
                    return createtype("NumberLiteral", amount or #rhs.EntryList, #leadingwhite <= 0)
                elseif amount <= 0 then
                    return createunop("#", rhs, leadingwhite)
                end

                local newex = createbinop("+", createtype("NumberLiteral", amount), createunop("#", rhs), leadingwhite)
                return newex
            end
        end

        if rhs.Type == "BooleanLiteral" then right = rSrc == "true" and true or false end
        if rhs.Type == "NumberLiteral" then
            right = tonumber(rSrc)
            if right == nil then return end
        end
        if rhs.Type == "StringLiteral" then right = rSrc:sub(2, #rSrc - 1) end

        if operator == "not" and rhs.Type ~= nil then
            if rhs.Type == "NilLiteral" or (rhs.Type == "BooleanLiteral" and right == false) then return true end
            return false
        end

        if right ~= nil then
            if operator == "#" then return #right end
            if operator == "-" then return -right end
        end
    end

    local solveStat, solveExpr;

    function solveExpr(expr)
        --warn("MATHSOLVE: SOLVING EXPR: ", expr)
        if expr.Type == "BinopExpr" then
            solveExpr(expr.Lhs)
            solveExpr(expr.Rhs)

            if expr.Lhs ~= nil and expr.Rhs ~= nil then
                local firsttoken = expr:GetFirstToken()
                local tokenOp = expr.Token_Op

                if tokenOp ~= nil and tokenOp.Source ~= nil and firsttoken then
                    local val = solvebinop(tokenOp.Source, expr.Lhs, expr.Rhs)

                    --dbgprint("returns of binop solve:")
                    --dbgprinttab(val)

                    if val ~= nil then
                        if type(val) == "boolean" then
                            local b = createtype("BooleanLiteral", tostring(val), "Keyword", #firsttoken.LeadingWhite <= 0)
                            replace(expr, b)
                            return
                        elseif type(val) == "number" then
                            if isfinite(val) then
                                local num = createtype("NumberLiteral", tostring(val), "Number", #firsttoken.LeadingWhite <= 0)
                                replace(expr, num)
                                return
                            end
                        elseif type(val) == "string" then
                            local str = createtype("StringLiteral", val, "String", #firsttoken.LeadingWhite <= 0)
                            replace(expr, str)
                            return
                        elseif type(val) == "table" then
                            replace(expr, val)
                            return
                        end
                        return
                    end
                end

                if expr.Lhs.Type == "ParenExpr" then
                    local exprt = expr.Lhs
                    local expression = exprt.Expression
                    if expression.Type == "NumberLiteral" or expression.Type == "StringLiteral"
                        or expression.Type == "NilLiteral" or expression.Type == "BooleanLiteral" or expression.Type == 'HashLiteral' then
                    end
                end
                if expr.Rhs.Type == "ParenExpr" then
                    local exprt = expr.Rhs
                    local expression = exprt.Expression
                    if expression.Type == "NumberLiteral" or expression.Type == "StringLiteral"
                        or expression.Type == "NilLiteral" or expression.Type == "BooleanLiteral" or expression.Type == 'HashLiteral' then
                    end
                end
            end
        elseif expr.Type == "UnopExpr" then
            --solveExpr(expr.Rhs) --causes infinite loop on `myvar = -myvar` (alteast from my experience), so i moved it down

            if expr.Rhs ~= nil and canSolve[expr.Rhs.Type] == true then
                solveExpr(expr.Rhs) --ofcourse solve it

                local tokenOp = expr.Token_Op

                if tokenOp ~= nil and tokenOp.Source ~= nil then
                    local rhs = expr.Rhs.Expression or expr.Rhs
                    local val = solveunop(tokenOp.Source, rhs, tokenOp.LeadingWhite)

                    if val ~= nil then
                        if type(val) == "boolean" then
                            local b = createtype("BooleanLiteral", tostring(val), "Keyword")
                            replace(expr, b)
                            return
                        elseif type(val) == "number" then
                            if isfinite(val) then
                                local num = createtype("NumberLiteral", tostring(val), "Number")
                                replace(expr, num)
                                return
                            end
                        elseif type(val) == "string" then
                            local str = createtype("StringLiteral", val, "String")
                            replace(expr, str)
                            return
                        elseif type(val) == "table" then
                            replace(expr, val)
                            return
                        end
                        return
                    end
                end
            end
        elseif (expr.Type == "NumberLiteral" or expr.Type == "StringLiteral"
            or expr.Type == "NilLiteral" or expr.Type == "BooleanLiteral"
            or expr.Type == "VargLiteral" or expr.Type == 'HashLiteral') then
            local token = expr.Token
            if token ~= nil then
                if token.Type == "Number" then
                    local int = {}
                    for part in token.Source:gmatch('([^e]+)') do
                        int[#int + 1] = part
                    end	
                    if #int == 2 then
                        --dbgprint("EXPONENT: ", token.Source)
                        local l = tonumber(int[1])
                        local r = tonumber(int[2])
                        if l and r then
                            if isfinite(l) and isfinite(r) and (l ^ r) < 999999999 and (not token.Source:find('+') and token.Source:find('.') and not token.Source:find('-')) then
                                token.Source = tostring(l ^ r)
                            end
                        end
                    end
                end				

                if token.Type == "String" then
                    token.Source = token.Source:gsub("\\\\%d+", function(got)
                        local num = tonumber(got:sub(2, #got - 1))

                        if num and isfinite(num) and (
                            (num >= 97 and num <= 122)
                                or (num >= 65 and num <= 90)
                                or (num >= 33 and num <= 47)
                                or (num >= 58 and num <= 64)
                                or (num >= 91 and num <= 96)
                                or (num >= 123 and num <= 126)
                            ) and num ~= 34 and num ~= 39 and num ~= 92 then
                            return string.char(num)
                        end

                        return got
                    end)
                end
            end
        elseif expr.Type == "TableLiteral" then
            for i, entry in ipairs(expr.EntryList) do
                if entry.EntryType == "Field" then
                    solveExpr(entry.Value)
                elseif entry.EntryType == "Index" then
                    solveExpr(entry.Index)
                    solveExpr(entry.Value)
                elseif entry.EntryType == "Value" then
                    solveExpr(entry.Value)
                else
                    error("unreachable")
                end
            end
        elseif expr.Type == "CallExpr" or expr.Type == "MethodExpr" then
            local base = expr.Base -- `Base(Arg1, Arg2)`

            --Solve the exprs of the arguments
            if expr.FunctionArguments then
                if expr.FunctionArguments.ArgList then
                    for i, ch in ipairs(expr.FunctionArguments.ArgList) do
                        if ch == nil or ch.Type == nil then
                            return
                        end
                        solveExpr(ch)
                    end
                end
            end

            --Setmetatable anti solve, we dont solve first arg as its gonna be a metatable
            if base.Type == "VariableExpr" then
                local basesolved = resolveliteral(base, true) or base
                local var = basesolved.Variable
                if var and var.Info.Type == "Global" then
                    if var.Info.Name == "setmetatable" then --Yeah now we push the "anti" onto the first argument, which is gonna be the `creatingmt`
                        local creatingmt = expr.FunctionArguments.ArgList[1]
                        if creatingmt.Type == "VariableExpr" and creatingmt.Variable then
                            --dbgprint("saved the mt!")
                            --dbgprinttab(creatingmt)
                            creatingmt.Variable.Info.LiteralStack[base.Variable.Location] = {expr, var.Scope}
                        end
                    end
                end
            end
        elseif expr.Type == "FunctionLiteral" then
            solveStat(expr.Body)
        elseif expr.Type == "ParenExpr" then
            solveExpr(expr.Expression)
        elseif expr.Type == "IndexExpr" or expr.Type == "FieldExpr" then
            local indexorfield = expr.Index or expr.Field -- `Base[Index]` OR `Base.Field`

            solveExpr(expr.Base)
            solveExpr(indexorfield)

            if solveindexes and indexorfield and (canSolve[indexorfield.Type] or indexorfield.Type == "Ident") then
                local literaltable = resolveliteral(expr.Base, true) --Resolve the literal of the Base
                if literaltable and literaltable.Type == "TableLiteral" then
                    local entrylist = literaltable.EntryList

                    local indexsource do --Get the source from the field or index type (More to be added soon)
                        if indexorfield.Type == "NumberLiteral" then
                            indexsource = tonumber(indexorfield.Token.Source)
                        elseif indexorfield.Type == "StringLiteral" then
                            indexsource = indexorfield.Token.Source:sub(2, -2) --only have the actual string
                        elseif indexorfield.Type == "Ident" then
                            indexsource = indexorfield.Source -- for field expr
                        end
                    end


                    local entryValue --Get the Entry's value
                    for index, entry in ipairs(entrylist) do
                        if entry.EntryType == "Value" and index == indexsource then
                            entryValue = entry.Value
                            break
                        elseif entry.EntryType == "Field" and entry.Field then
                            if entry.Field.Type == "Ident" and entry.Field.Source == indexsource then
                                entryValue = entry.Value
                                break
                            end
                        end
                    end

                    if entryValue and canSolve[indexorfield.Type] then
                        local entryclone = clonedeep(entryValue) --BE CAREFUL, luckily this never references "upward", so no loops.
                        --dbgprint("successfully replaced index!")
                        local beforeleadingwhite = expr:GetFirstToken().LeadingWhite
                        replace(expr, entryclone)
                        entryclone:GetFirstToken().LeadingWhite = beforeleadingwhite --make sure to transfer leadingwhite
                        --dbgprinttab(expr)
                    end
                end
            end
        end
    end

    function solveStat(stat)
        if stat.Type == "StatList" then
            for i, ch in ipairs(stat.StatementList) do
                if ch == nil or ch.Type == nil then
                    return
                end

                --Add some functions of the statements to interact with the statement list

                ch.NewStat = function(Stat, RelativePos)
                    table.insert(stat.StatementList, i + RelativePos, Stat)
                end

                ch.Remove = function()
                    local firstoken = stat.StatementList[i]:GetFirstToken()
                    local nextstat = stat.StatementList[i + 1]
                    if firstoken and nextstat then
                        local leadwhite = firstoken.LeadingWhite
                        local nexttoken = nextstat:GetFirstToken()
                        if nexttoken and leadwhite then --Pass on leading white
                            if leadwhite:sub(#leadwhite) == "\n" then --remove new line from the removed statement
                                leadwhite = leadwhite:sub(1, #leadwhite - 1)
                            end
                            nexttoken.LeadingWhite = leadwhite .. nexttoken.LeadingWhite
                        end
                    end
                    --print(leadwhite)

                    stat.StatementList[i] = nil
                end	

                solveStat(ch)
            end
        elseif stat.Type == "BreakStat" then
            --nothing
        elseif stat.Type == "ContinueStat" then
            --what
        elseif stat.Type == "ReturnStat" then
            for i, expr in ipairs(stat.ExprList) do
                solveExpr(expr)
            end
        elseif stat.Type == "LocalVarStat" then
            if stat.Token_Equals ~= nil then
                for i, expr in ipairs(stat.ExprList) do
                    solveExpr(expr)
                end
            end
        elseif stat.Type == "LocalFunctionStat" then
            solveStat(stat.FunctionStat.Body)

            if #stat.FunctionStat.NameChain == 1 then
                if stat.FunctionStat.NameChain[1].UseCount == 0 then
                    return stat.Remove()
                end
            end

        elseif stat.Type == "FunctionStat" then
            solveStat(stat.Body)
        elseif stat.Type == "RepeatStat" then
            solveStat(stat.Body)
            solveExpr(stat.Condition)

            if stat.Body.Type == "StatList" and #stat.Body.StatementList == 0 then
                return stat.Remove()
            end
        elseif stat.Type == "GenericForStat" then
            for i, expr in ipairs(stat.GeneratorList) do
                solveExpr(expr)
            end
            solveStat(stat.Body)
        elseif stat.Type == "NumericForStat" then
            for i, expr in ipairs(stat.RangeList) do
                solveExpr(expr)
            end
            solveStat(stat.Body)

            local a = stat.RangeList[1]
            local b = stat.RangeList[2]
            local c = stat.RangeList[3]
            if a == nil or b == nil then
                return stat.Remove()
            end

            removeParen(a)
            removeParen(b)
            removeParen(c)

            if a.Type ~= "NumberLiteral" or b.Type ~= "NumberLiteral" or c ~= nil and (c.Type ~= "NumberLiteral" or c == nil) then
                return
            end

            local start = tonumber(a.Token.Source)
            local endd = tonumber(b.Token.Source)
            local step = (c ~= nil and tonumber(c.Token.Source)) or 1

            local t1 = ((step > 0 and start <= endd) or (step < 0 and start >= endd))
            local t2 = ((endd - start) + step) / step

            local willRun = t1 and t2 >= 0

            if not willRun then
                return stat.Remove()
            end

            if stat.Body.Type == "StatList" and #stat.Body.StatementList == 0 then
                return stat.Remove()
            end
        elseif stat.Type == "WhileStat" then
            solveExpr(stat.Condition)
            solveStat(stat.Body)

            local condition = stat.Condition
            if condition.Type == "ParenExpr" then
                condition = condition.Expression
            end
            if condition.Type == "BooleanLiteral" then
                if condition == nil or condition.Token == nil or condition.Token.Source ~= "false" then
                end
            elseif condition.Type == "NilLiteral" then
                return stat.Remove()
            end
        elseif stat.Type == "DoStat" then
            solveStat(stat.Body)

            if stat.Body == nil or stat.Body.Type == "StatList" and #stat.Body.StatementList == 0 then
                return stat.Remove()
            elseif #stat.Body.StatementList == 1 then
                local s = stat.Body.StatementList[1]
                if s.Type ~= 'ContinueStat'
                    and s.Type ~= 'BreakStat'
                    and s.Type ~= 'ReturnStat' then
                    replace(stat, s)
                end
            end
        elseif stat.Type == "IfStat" then
            --print("CONDIT IFSTAT BEFORE: ", stat.Condition)

            solveExpr(stat.Condition)
            --print("CONDIT IFSTAT BEFORE2: ", stat.Condition)
            solveStat(stat.Body)
            for i, clause in ipairs(stat.ElseClauseList) do
                if clause.Condition ~= nil then
                    solveExpr(clause.Condition)
                end
                solveStat(clause.Body)
            end

            local condition = stat.Condition
            if condition.Type == "ParenExpr" then
                condition = condition.Expression
            end
            if condition.Type == "BooleanLiteral" and solveifstats then
                if condition.Token.Source == "false" then --its useless what lol
                    if #stat.ElseClauseList > 1 then
                        if stat.ElseClauseList[1] and stat.ElseClauseList[1].Condition then --Dont work about just `else` as those should never occur.
                            local newifstat = clone(stat)

                            --replace clause information
                            local firstelseclause = newifstat.ElseClauseList[1]

                            --newifstat.Token_Then = firstelseclause.Token_Then
                            newifstat.Body = firstelseclause.Body
                            newifstat.Condition = firstelseclause.Condition
                            table.remove(newifstat.ElseClauseList, 1)

                            stat.Remove()
                            return stat.NewStat(newifstat, 1)
                        end
                    else
                        local lastclause = stat.ElseClauseList[#stat.ElseClauseList]
                        if lastclause and not lastclause.Condition then
                            --dbgprint("Attempting to replace token")
                            --Replace last elseclause as the main will always be performed.
                            replace(stat, { --__here
                                Type = 'DoStat';
                                Body = lastclause.Body;
                                Token_Do = {
                                    LeadingWhite = stat:GetFirstToken().LeadingWhite or "",
                                    Source = "do",
                                    Type = "Keyword"
                                };
                                GetFirstToken = function(self)
                                    return self.Token_Do
                                end;
                                GetLastToken = function(self)
                                    return self.Token_End
                                end;
                            })
                        else
                            stat.Remove() --remove it lol
                        end
                    end
                elseif condition.Token.Source == "true" then
					--[[ --My dumb ahh just realized all this is useless...
					if stat.ElseClauseList then
						if stat.ElseClauseList[1] and stat.ElseClauseList[1].Condition then --Dont work about just `else` as those should never occur.
							local newifstat = copy(stat)
	
							--replace clause information
							local firstelseclause = newifstat.ElseClauseList[1]
							--newifstat.Token_Then = firstelseclause.Token_Then
							newifstat.Body = firstelseclause.Body
							newifstat.Condition = firstelseclause.Condition
							table.remove(newifstat.ElseClauseList, 1)
	
							stat.NewStat(newifstat, 1)
						end
						local lastclause = stat.ElseClauseList[#stat.ElseClauseList]
						if lastclause and not lastclause.Condition then
							table.remove(stat.ElseClauseList) --Remove last elseclause, as the else will never be performed.
						end
					end
					]]
                    replace(stat, { --__here
                        Type = 'DoStat';
                        Body = stat.Body;
                        Token_Do = {
                            LeadingWhite = stat.Token_If.LeadingWhite or "",
                            Source = "do",
                            Type = "Keyword"
                        };
                        GetFirstToken = function(self)
                            return self.Token_Do
                        end;
                        GetLastToken = function(self)
                            return self.Token_End
                        end;
                    })
                end
            elseif condition.Type == "NilLiteral" then
                return stat.Remove()
            end

            if solveifstats then
                for i = 1, #stat.ElseClauseList do
                    local clause = stat.ElseClauseList[i]
                    if clause then
                        local condition = clause.Condition
                        if condition and condition.Token and stat.ElseClauseList[i].Token and condition.Type == "BooleanLiteral" then
                            if condition.Token.Source == "true" then
                                repeat
                                    table.remove(stat.ElseClauseList, i + 1)
                                until not stat.ElseClauseList[i + 1]
                                stat.ElseClauseList[i].Condition = nil
                                stat.ElseClauseList[i].ClauseType = "else"
                                stat.ElseClauseList[i].Token.Source = "else"
                            elseif condition.Token.Source == "false" then
                                table.remove(stat.ElseClauseList, i)
                            else
                                error("invalid source to boolean value `"..tostring(condition.Token.Source).."`")
                            end
                        end
                    end
                end
            end
        elseif stat.Type == "CallExprStat" then
            solveExpr(stat.Expression)
        elseif stat.Type == "CompoundStat" then
            solveExpr(stat.Lhs)
            solveExpr(stat.Rhs)
        elseif stat.Type == "AssignmentStat" then
            for i, ex in pairs(stat.Lhs) do
                solveExpr(ex)
            end
            for i, ex in ipairs(stat.Rhs) do
                solveExpr(ex)
                local lhsval = stat.Lhs[i]
                local rhsval = ex
                if lhsval.Variable then
                    local curlocation = lhsval.Variable.Location
                    --if rhsval.Type ~= "BinopExpr" then
                    lhsval.Variable.Info.LiteralStack[curlocation] = {rhsval, lhsval.Variable.Scope}
                    --end
                    --ex.Variable.LiteralVal = stat.Rhs[i]
                end
            end
        else
            error("unfound: "..tostring(stat.Type))
        end
    end

    solveStat(ast)
    return ast
end

-- Outputs out an AST to a string, then returns that string
function StringAst(ast)

    local result = ""
    local printStat, printExpr;

    local function printt(tk)
        if not tk.LeadingWhite or not tk.Source then
            error("Bad token: "..FormatTable(tk))
        end
        result = result .. (tk.LeadingWhite) .. (tk.Source)
    end

    printExpr = function(expr)
        if expr.Type == 'BinopExpr' then
            printExpr(expr.Lhs)
            printt(expr.Token_Op)
            printExpr(expr.Rhs)
        elseif expr.Type == 'UnopExpr' then
            printt(expr.Token_Op)
            printExpr(expr.Rhs)
        elseif expr.Type == 'NumberLiteral' or expr.Type == 'StringLiteral' or 
            expr.Type == 'NilLiteral' or expr.Type == 'BooleanLiteral' or 
            expr.Type == 'VargLiteral' 
        then
            -- Just print the token
            printt(expr.Token)
        elseif expr.Type == 'FieldExpr' then
            printExpr(expr.Base)
            printt(expr.Token_Dot)
            printt(expr.Field)
        elseif expr.Type == 'IndexExpr' then
            printExpr(expr.Base)
            printt(expr.Token_OpenBracket)
            printExpr(expr.Index)
            printt(expr.Token_CloseBracket)
        elseif expr.Type == 'MethodExpr' or expr.Type == 'CallExpr' then
            printExpr(expr.Base)
            if expr.Type == 'MethodExpr' then
                printt(expr.Token_Colon)
                printt(expr.Method)
            end
            if expr.FunctionArguments.CallType == 'StringCall' then
                printt(expr.FunctionArguments.Token)
            elseif expr.FunctionArguments.CallType == 'ArgCall' then
                printt(expr.FunctionArguments.Token_OpenParen)
                for index, argExpr in pairs(expr.FunctionArguments.ArgList) do
                    printExpr(argExpr)
                    local sep = expr.FunctionArguments.Token_CommaList[index]
                    if sep then
                        printt(sep)
                    end
                end
                printt(expr.FunctionArguments.Token_CloseParen)
            elseif expr.FunctionArguments.CallType == 'TableCall' then
                printExpr(expr.FunctionArguments.TableExpr)
            end
        elseif expr.Type == 'FunctionLiteral' then
            printt(expr.Token_Function)
            printt(expr.Token_OpenParen)
            for index, arg in pairs(expr.ArgList) do
                printt(arg)
                local comma = expr.Token_ArgCommaList[index]
                if comma then
                    printt(comma)
                end
            end
            printt(expr.Token_CloseParen)
            printStat(expr.Body)
            printt(expr.Token_End)
        elseif expr.Type == 'VariableExpr' then
            printt(expr.Token)
        elseif expr.Type == 'ParenExpr' then
            printt(expr.Token_OpenParen)
            --print(expr.Token_OpenParen)
            printExpr(expr.Expression)
            printt(expr.Token_CloseParen)
        elseif expr.Type == 'TableLiteral' then
            printt(expr.Token_OpenBrace)
            for index, entry in pairs(expr.EntryList) do
                if entry.EntryType == 'Field' then
                    printt(entry.Field)
                    printt(entry.Token_Equals)
                    printExpr(entry.Value)
                elseif entry.EntryType == 'Index' then
                    printt(entry.Token_OpenBracket)
                    printExpr(entry.Index)
                    printt(entry.Token_CloseBracket)
                    printt(entry.Token_Equals)
                    printExpr(entry.Value)
                elseif entry.EntryType == 'Value' then
                    printExpr(entry.Value)
                else
                    error("unreachable")
                end
                local sep = expr.Token_SeparatorList[index]
                if sep then
                    printt(sep)
                end
            end
            printt(expr.Token_CloseBrace)
        else
            error("unreachable, type: "..expr.Type..":"..FormatTable(expr))
        end
    end

    printStat = function(stat)
        if stat.Type == 'StatList' then
            for index, ch in pairs(stat.StatementList) do
                printStat(ch)
                if stat.SemicolonList[index] then
                    printt(stat.SemicolonList[index])
                end
            end
        elseif stat.Type == 'BreakStat' then
            printt(stat.Token_Break)
        elseif stat.Type == 'ReturnStat' then
            printt(stat.Token_Return)
            for index, expr in pairs(stat.ExprList) do
                printExpr(expr)
                if stat.Token_CommaList[index] then
                    printt(stat.Token_CommaList[index])
                end
            end
        elseif stat.Type == 'LocalVarStat' then
            printt(stat.Token_Local)
            for index, var in pairs(stat.VarList) do
                printt(var)
                local comma = stat.Token_VarCommaList[index]
                if comma then
                    printt(comma)
                end
            end
            if stat.Token_Equals then
                printt(stat.Token_Equals)
                for index, expr in pairs(stat.ExprList) do
                    printExpr(expr)
                    local comma = stat.Token_ExprCommaList[index]
                    if comma then
                        printt(comma)
                    end
                end
            end
        elseif stat.Type == 'LocalFunctionStat' then
            printt(stat.Token_Local)
            printt(stat.FunctionStat.Token_Function)
            printt(stat.FunctionStat.NameChain[1])
            printt(stat.FunctionStat.Token_OpenParen)
            for index, arg in pairs(stat.FunctionStat.ArgList) do
                printt(arg)
                local comma = stat.FunctionStat.Token_ArgCommaList[index]
                if comma then
                    printt(comma)
                end
            end
            printt(stat.FunctionStat.Token_CloseParen)
            printStat(stat.FunctionStat.Body)
            printt(stat.FunctionStat.Token_End)
        elseif stat.Type == 'FunctionStat' then
            printt(stat.Token_Function)
            for index, part in pairs(stat.NameChain) do
                printt(part)
                local sep = stat.Token_NameChainSeparator[index]
                if sep then
                    printt(sep)
                end
            end
            printt(stat.Token_OpenParen)
            for index, arg in pairs(stat.ArgList) do
                printt(arg)
                local comma = stat.Token_ArgCommaList[index]
                if comma then
                    printt(comma)
                end
            end
            printt(stat.Token_CloseParen)
            printStat(stat.Body)
            printt(stat.Token_End)
        elseif stat.Type == 'RepeatStat' then
            printt(stat.Token_Repeat)
            printStat(stat.Body)
            printt(stat.Token_Until)
            printExpr(stat.Condition)
        elseif stat.Type == 'GenericForStat' then
            printt(stat.Token_For)
            for index, var in pairs(stat.VarList) do
                printt(var)
                local sep = stat.Token_VarCommaList[index]
                if sep then
                    printt(sep)
                end
            end
            printt(stat.Token_In)
            for index, expr in pairs(stat.GeneratorList) do
                printExpr(expr)
                local sep = stat.Token_GeneratorCommaList[index]
                if sep then
                    printt(sep)
                end
            end
            printt(stat.Token_Do)
            printStat(stat.Body)
            printt(stat.Token_End)
        elseif stat.Type == 'NumericForStat' then
            printt(stat.Token_For)
            for index, var in pairs(stat.VarList) do
                printt(var)
                local sep = stat.Token_VarCommaList[index]
                if sep then
                    printt(sep)
                end
            end
            printt(stat.Token_Equals)
            for index, expr in pairs(stat.RangeList) do
                printExpr(expr)
                local sep = stat.Token_RangeCommaList[index]
                if sep then
                    printt(sep)
                end
            end
            printt(stat.Token_Do)
            printStat(stat.Body)
            printt(stat.Token_End)		
        elseif stat.Type == 'WhileStat' then
            printt(stat.Token_While)
            printExpr(stat.Condition)
            printt(stat.Token_Do)
            printStat(stat.Body)
            printt(stat.Token_End)
        elseif stat.Type == 'DoStat' then
            printt(stat.Token_Do)
            printStat(stat.Body)
            printt(stat.Token_End)
        elseif stat.Type == 'IfStat' then
            printt(stat.Token_If)
            printExpr(stat.Condition)
            printt(stat.Token_Then)
            printStat(stat.Body)
            for _, clause in pairs(stat.ElseClauseList) do
                printt(clause.Token)
                if clause.Condition then
                    printExpr(clause.Condition)
                    printt(clause.Token_Then)
                end
                printStat(clause.Body)
            end
            printt(stat.Token_End)
        elseif stat.Type == 'CallExprStat' then
            printExpr(stat.Expression)
        elseif stat.Type == 'AssignmentStat' then
            for index, ex in pairs(stat.Lhs) do
                printExpr(ex)
                local sep = stat.Token_LhsSeparatorList[index]
                if sep then
                    printt(sep)
                end
            end
            printt(stat.Token_Equals)
            for index, ex in pairs(stat.Rhs) do
                printExpr(ex)
                local sep = stat.Token_RhsSeparatorList[index]
                if sep then
                    printt(sep)
                end
            end
        else
            error("unreachable")
        end
    end

    printStat(ast)

    return result
end

--#endregion

--#region Formatters

-- Adds / removes whitespace in an AST to put it into a "standard formatting"
local function FormatAst(ast)
    local formatStat, formatExpr;

    local currentIndent = 0

    local function applyIndent(token)
        local indentString = '\n'..('\t'):rep(currentIndent)
        if token.LeadingWhite == '' or (token.LeadingWhite:sub(-#indentString, -1) ~= indentString) then
            -- Trim existing trailing whitespace on LeadingWhite
            -- Trim trailing tabs and spaces, and up to one newline
            token.LeadingWhite = token.LeadingWhite:gsub("\n?[\t ]*$", "")
            token.LeadingWhite = token.LeadingWhite..indentString
        end
    end

    local function indent()
        currentIndent = currentIndent + 1
    end

    local function undent()
        currentIndent = currentIndent - 1
        assert(currentIndent >= 0, "Undented too far")
    end

    local function leadingChar(tk)
        --dbgprint("TOKEN:" , tk)
        if #tk.LeadingWhite > 0 then
            return tk.LeadingWhite:sub(1,1)
        else
            return tk.Source:sub(1,1)
        end
    end

    local function padToken(tk)
        if not WhiteChars[leadingChar(tk)] then
            tk.LeadingWhite = ' '..tk.LeadingWhite
        end
    end

    local function padExpr(expr)
        padToken(expr:GetFirstToken() or expr.Token or dbgprint("couldnt get a token -->", expr)) --used to do `expr:GetFirstToken()`
    end

    local function formatBody(openToken, bodyStat, closeToken)
        indent()
        formatStat(bodyStat)
        undent()
        applyIndent(closeToken)
    end

    formatExpr = function(expr)
        if expr.Type == 'BinopExpr' then
            formatExpr(expr.Lhs)
            formatExpr(expr.Rhs)
            if expr.Token_Op.Source == '..' then
                -- No padding on ..
            else
                padExpr(expr.Rhs)
                padToken(expr.Token_Op)
            end
        elseif expr.Type == 'UnopExpr' then
            --a.r = lol
            formatExpr(expr.Rhs)
            --(expr.Token_Op)
        elseif expr.Type == 'NumberLiteral' or expr.Type == 'StringLiteral' or 
            expr.Type == 'NilLiteral' or expr.Type == 'BooleanLiteral' or 
            expr.Type == 'VargLiteral' 
        then
            -- Nothing to do
            --(expr.Token)
        elseif expr.Type == 'FieldExpr' then
            formatExpr(expr.Base)
            --(expr.Token_Dot)
            --(expr.Field)
        elseif expr.Type == 'IndexExpr' then
            formatExpr(expr.Base)
            formatExpr(expr.Index)
            --(expr.Token_OpenBracket)
            --(expr.Token_CloseBracket)
        elseif expr.Type == 'MethodExpr' or expr.Type == 'CallExpr' then
            formatExpr(expr.Base)
            if expr.Type == 'MethodExpr' then
                --(expr.Token_Colon)
                --(expr.Method)
            end
            if expr.FunctionArguments.CallType == 'StringCall' then
                --(expr.FunctionArguments.Token)
            elseif expr.FunctionArguments.CallType == 'ArgCall' then
                --(expr.FunctionArguments.Token_OpenParen)
                for index, argExpr in pairs(expr.FunctionArguments.ArgList) do
                    formatExpr(argExpr)
                    if index > 1 then
                        padExpr(argExpr)
                    end
                    local sep = expr.FunctionArguments.Token_CommaList[index]
                    if sep then
                        --(sep)
                    end
                end
                --(expr.FunctionArguments.Token_CloseParen)
            elseif expr.FunctionArguments.CallType == 'TableCall' then
                formatExpr(expr.FunctionArguments.TableExpr)
            end
        elseif expr.Type == 'FunctionLiteral' then
            --(expr.Token_Function)
            --(expr.Token_OpenParen)
            for index, arg in pairs(expr.ArgList) do
                --(arg)
                if index > 1 then
                    padToken(arg)
                end
                local comma = expr.Token_ArgCommaList[index]
                if comma then
                    --(comma)
                end
            end
            --(expr.Token_CloseParen)
            formatBody(expr.Token_CloseParen, expr.Body, expr.Token_End)
        elseif expr.Type == 'VariableExpr' then
            --(expr.Token)
        elseif expr.Type == 'ParenExpr' then
            formatExpr(expr.Expression)
            --(expr.Token_OpenParen)
            --(expr.Token_CloseParen)
        elseif expr.Type == 'TableLiteral' then
            --(expr.Token_OpenBrace)
            if #expr.EntryList == 0 then
                -- Nothing to do
            else
                indent()
                for index, entry in pairs(expr.EntryList) do
                    if entry.EntryType == 'Field' then
                        applyIndent(entry.Field)
                        padToken(entry.Token_Equals)
                        formatExpr(entry.Value)
                        padExpr(entry.Value)
                    elseif entry.EntryType == 'Index' then
                        applyIndent(entry.Token_OpenBracket)
                        formatExpr(entry.Index)
                        --(entry.Token_CloseBracket)
                        padToken(entry.Token_Equals)
                        formatExpr(entry.Value)
                        padExpr(entry.Value)
                    elseif entry.EntryType == 'Value' then
                        formatExpr(entry.Value)
                        applyIndent(entry.Value:GetFirstToken())
                    else
                        error("unreachable")
                    end
                    local sep = expr.Token_SeparatorList[index]
                    if sep then
                        --(sep)
                    end
                end
                undent()
                applyIndent(expr.Token_CloseBrace)
            end
            --(expr.Token_CloseBrace)
        else
            error("unreachable, type: "..expr.Type..":"..FormatTable(expr))
        end
    end

    formatStat = function(stat)
        if stat.Type == 'StatList' then
            for _, stat in pairs(stat.StatementList) do
                formatStat(stat)
                applyIndent(stat:GetFirstToken())
            end

        elseif stat.Type == 'BreakStat' then
            --(stat.Token_Break)

        elseif stat.Type == 'ReturnStat' then
            --(stat.Token_Return)
            for index, expr in pairs(stat.ExprList) do
                formatExpr(expr)
                padExpr(expr)
                if stat.Token_CommaList[index] then
                    --(stat.Token_CommaList[index])
                end
            end
        elseif stat.Type == 'LocalVarStat' then
            --(stat.Token_Local)
            for index, var in pairs(stat.VarList) do
                padToken(var)
                local comma = stat.Token_VarCommaList[index]
                if comma then
                    --(comma)
                end
            end
            if stat.Token_Equals then
                padToken(stat.Token_Equals)
                for index, expr in pairs(stat.ExprList) do
                    formatExpr(expr)
                    padExpr(expr)
                    local comma = stat.Token_ExprCommaList[index]
                    if comma then
                        --(comma)
                    end
                end
            end
        elseif stat.Type == 'LocalFunctionStat' then
            --(stat.Token_Local)
            padToken(stat.FunctionStat.Token_Function)
            padToken(stat.FunctionStat.NameChain[1])
            --(stat.FunctionStat.Token_OpenParen)
            for index, arg in pairs(stat.FunctionStat.ArgList) do
                if index > 1 then
                    padToken(arg)
                end
                local comma = stat.FunctionStat.Token_ArgCommaList[index]
                if comma then
                    --(comma)
                end
            end
            --(stat.FunctionStat.Token_CloseParen)
            formatBody(stat.FunctionStat.Token_CloseParen, stat.FunctionStat.Body, stat.FunctionStat.Token_End)
        elseif stat.Type == 'FunctionStat' then
            --(stat.Token_Function)
            for index, part in pairs(stat.NameChain) do
                if index == 1 then
                    padToken(part)
                end
                local sep = stat.Token_NameChainSeparator[index]
                if sep then
                    --(sep)
                end
            end
            --(stat.Token_OpenParen)
            for index, arg in pairs(stat.ArgList) do
                if index > 1 then
                    padToken(arg)
                end
                local comma = stat.Token_ArgCommaList[index]
                if comma then
                    --(comma)
                end
            end
            --(stat.Token_CloseParen)
            formatBody(stat.Token_CloseParen, stat.Body, stat.Token_End)
        elseif stat.Type == 'RepeatStat' then
            --(stat.Token_Repeat)
            formatBody(stat.Token_Repeat, stat.Body, stat.Token_Until)
            formatExpr(stat.Condition)
            padExpr(stat.Condition)
        elseif stat.Type == 'GenericForStat' then
            --(stat.Token_For)
            for index, var in pairs(stat.VarList) do
                padToken(var)
                local sep = stat.Token_VarCommaList[index]
                if sep then
                    --(sep)
                end
            end
            padToken(stat.Token_In)
            for index, expr in pairs(stat.GeneratorList) do
                formatExpr(expr)
                padExpr(expr)
                local sep = stat.Token_GeneratorCommaList[index]
                if sep then
                    --(sep)
                end
            end
            padToken(stat.Token_Do)
            formatBody(stat.Token_Do, stat.Body, stat.Token_End)
        elseif stat.Type == 'NumericForStat' then
            --(stat.Token_For)
            for index, var in pairs(stat.VarList) do
                padToken(var)
                local sep = stat.Token_VarCommaList[index]
                if sep then
                    --(sep)
                end
            end
            padToken(stat.Token_Equals)
            for index, expr in pairs(stat.RangeList) do
                formatExpr(expr)
                padExpr(expr)
                local sep = stat.Token_RangeCommaList[index]
                if sep then
                    --(sep)
                end
            end
            padToken(stat.Token_Do)
            formatBody(stat.Token_Do, stat.Body, stat.Token_End)	
        elseif stat.Type == 'WhileStat' then
            --(stat.Token_While)
            formatExpr(stat.Condition)
            padExpr(stat.Condition)
            padToken(stat.Token_Do)
            formatBody(stat.Token_Do, stat.Body, stat.Token_End)
        elseif stat.Type == 'DoStat' then
            --warn("DOSTAT: ", stat)
            formatBody(stat.Token_Do, stat.Body, stat.Token_End)
        elseif stat.Type == 'IfStat' then
            --(stat.Token_If)
            --print(stat.Condition)
            formatExpr(stat.Condition)
            padExpr(stat.Condition)
            padToken(stat.Token_Then)
            --
            local lastBodyOpen = stat.Token_Then
            local lastBody = stat.Body
            --
            for _, clause in pairs(stat.ElseClauseList) do
                formatBody(lastBodyOpen, lastBody, clause.Token)
                lastBodyOpen = clause.Token
                --
                if clause.Condition then
                    formatExpr(clause.Condition)
                    padExpr(clause.Condition)
                    padToken(clause.Token_Then)
                    lastBodyOpen = clause.Token_Then
                end
                lastBody = clause.Body
            end
            --
            formatBody(lastBodyOpen, lastBody, stat.Token_End)

        elseif stat.Type == 'CallExprStat' then
            formatExpr(stat.Expression)
        elseif stat.Type == 'AssignmentStat' then
            for index, ex in pairs(stat.Lhs) do
                formatExpr(ex)
                if index > 1 then
                    padExpr(ex)
                end
                local sep = stat.Token_LhsSeparatorList[index]
                if sep then
                    --(sep)
                end
            end
            padToken(stat.Token_Equals)
            for index, ex in pairs(stat.Rhs) do
                formatExpr(ex)
                padExpr(ex)
                local sep = stat.Token_RhsSeparatorList[index]
                if sep then
                    --(sep)
                end
            end
        else
            error("unreachable")
        end	
    end
    formatStat(ast)
    --dbgprint("RESULT AST: ", ast)
    return ast
end

-- Strips as much whitespace off of tokens in an AST as possible without causing problems
local function StripAst(ast)
    local stripStat, stripExpr;


    --[[
        type Token = {
            Source: string,
            LeadingWhite: string,
            Type: string
        }
    ]]

    local function stript(token) --token: Token
        token.LeadingWhite = ''
    end

    local function striptfirsttokenstat(token) --token: Token
        if token.Source ~= "(" then
            token.LeadingWhite = ''
        else
            --dbgprint("Spotted ambigous syntax token `(`, adding semicolon..")
            token.LeadingWhite = ';'
        end
    end

    -- Make to adjacent tokens as close as possible
    local function joint(tokenA, tokenB, JointChar)
        -- Get the trailing A <-> leading B character pair
        local lastCh = tokenA.Source:sub(-1, -1)
        local firstCh = tokenB.Source:sub(1, 1)

        -- Cases to consider:
        --  Touching minus signs -> comment: `- -42` -> `--42' is invalid
        --  Touching dots: `.. .5` -> `...5` is invalid
        --  Touching words: `a b` -> `ab` is invalid
        --  Touching digits: `2 3`, can't occurr in the Lua syntax as number literals aren't a primary expression
        --  Abiguous syntax: `f(x)\n(x)()` is already disallowed, we can't cause a problem by removing newlines
        --  `>` `=` cannot be merged, because they will become a `>=` token.

        -- Figure out what separation is needed
        if 
            (lastCh == '-' and firstCh == '-') or
            (lastCh == '>' and firstCh == '=') or
            (lastCh == '.' and firstCh == '.') or
            (AllIdentChars[lastCh] and AllIdentChars[firstCh]) 
        then
            tokenB.LeadingWhite = JointChar or ' ' -- Use a separator
        else
            tokenB.LeadingWhite = '' -- Don't use a separator
        end
    end

    -- Join up a statement body and it's opening / closing tokens
    local function bodyjoint(open, body, close)
        stripStat(body)
        stript(close)
        local bodyFirst = body:GetFirstToken()
        local bodyLast = body:GetLastToken()
        if bodyFirst then
            -- Body is non-empty, join body to open / close
            joint(open, bodyFirst)
            joint(bodyLast, close)
        else
            -- Body is empty, just join open and close token together
            joint(open, close)
        end
    end

    stripExpr = function(expr, leadsstatment)
        if expr.Type == 'BinopExpr' then
            stripExpr(expr.Lhs)
            stript(expr.Token_Op)
            stripExpr(expr.Rhs)
            -- Handle the `a - -b` -/-> `a--b` case which would otherwise incorrectly generate a comment
            -- Also handles operators "or" / "and" which definitely need joining logic in a bunch of cases
            joint(expr.Token_Op, expr.Rhs:GetFirstToken())
            joint(expr.Lhs:GetLastToken(), expr.Token_Op)
        elseif expr.Type == 'UnopExpr' then
            stript(expr.Token_Op)
            stripExpr(expr.Rhs)
            -- Handle the `- -b` -/-> `--b` case which would otherwise incorrectly generate a comment
            joint(expr.Token_Op, expr.Rhs:GetFirstToken())
        elseif expr.Type == 'NumberLiteral' or expr.Type == 'StringLiteral' or 
            expr.Type == 'NilLiteral' or expr.Type == 'BooleanLiteral' or 
            expr.Type == 'VargLiteral' 
        then
            -- Just print the token
            stript(expr.Token)
        elseif expr.Type == 'FieldExpr' then
            stripExpr(expr.Base)
            stript(expr.Token_Dot)
            stript(expr.Field)
        elseif expr.Type == 'IndexExpr' then
            stripExpr(expr.Base)
            stript(expr.Token_OpenBracket)
            stripExpr(expr.Index)
            stript(expr.Token_CloseBracket)
        elseif expr.Type == 'MethodExpr' or expr.Type == 'CallExpr' then
            --dbgprint("leads:", leadsstatment)
            stripExpr(expr.Base, leadsstatment)
            if expr.Type == 'MethodExpr' then
                stript(expr.Token_Colon)
                stript(expr.Method)
            end
            if expr.FunctionArguments.CallType == 'StringCall' then
                stript(expr.FunctionArguments.Token)
            elseif expr.FunctionArguments.CallType == 'ArgCall' then
                stript(expr.FunctionArguments.Token_OpenParen)
                for index, argExpr in pairs(expr.FunctionArguments.ArgList) do
                    stripExpr(argExpr)
                    local sep = expr.FunctionArguments.Token_CommaList[index]
                    if sep then
                        stript(sep)
                    end
                end
                stript(expr.FunctionArguments.Token_CloseParen)
            elseif expr.FunctionArguments.CallType == 'TableCall' then
                stripExpr(expr.FunctionArguments.TableExpr)
            end
        elseif expr.Type == 'FunctionLiteral' then
            stript(expr.Token_Function)
            stript(expr.Token_OpenParen)
            for index, arg in pairs(expr.ArgList) do
                stript(arg)
                local comma = expr.Token_ArgCommaList[index]
                if comma then
                    stript(comma)
                end
            end
            stript(expr.Token_CloseParen)
            bodyjoint(expr.Token_CloseParen, expr.Body, expr.Token_End)
        elseif expr.Type == 'VariableExpr' then
            stript(expr.Token)
        elseif expr.Type == 'ParenExpr' then
            --print(leadsstatment)
            stript(expr.Token_OpenParen)
            stripExpr(expr.Expression)
            stript(expr.Token_CloseParen)
        elseif expr.Type == 'TableLiteral' then
            stript(expr.Token_OpenBrace)
            for index, entry in pairs(expr.EntryList) do
                if entry.EntryType == 'Field' then
                    stript(entry.Field)
                    stript(entry.Token_Equals)
                    stripExpr(entry.Value)
                elseif entry.EntryType == 'Index' then
                    stript(entry.Token_OpenBracket)
                    stripExpr(entry.Index)
                    stript(entry.Token_CloseBracket)
                    stript(entry.Token_Equals)
                    stripExpr(entry.Value)
                elseif entry.EntryType == 'Value' then
                    stripExpr(entry.Value)
                else
                    error("unreachable")
                end
                local sep = expr.Token_SeparatorList[index]
                if sep then
                    stript(sep)
                end
            end
            stript(expr.Token_CloseBrace)
        else
            error("unreachable, type: "..expr.Type..":"..FormatTable(expr))
        end
    end

    stripStat = function(stat)
        if stat.Type == 'StatList' then
            -- Strip all surrounding whitespace on statement lists along with separating whitespace
            for i = 1, #stat.StatementList do
                local chStat = stat.StatementList[i]

                -- Strip the statement and it's whitespace
                stripStat(chStat)
                --stript(chStat:GetFirstToken())
                --print("first token:", chStat:GetFirstToken(), "of ", chStat)

                -- If there was a last statement, join them appropriately
                local lastChStat = stat.StatementList[i-1]
                if lastChStat then
                    -- See if we can remove a semi-colon, the only case where we can't is if
                    -- this and the last statement have a `);(` pair, where removing the semi-colon
                    -- would introduce ambiguous syntax.
                    if stat.SemicolonList[i-1] and 
                        (lastChStat:GetLastToken().Source ~= ')' or chStat:GetFirstToken().Source ~= ')')
                    then
                        stat.SemicolonList[i-1] = nil
                    end

                    -- If there isn't a semi-colon, we should safely join the two statements
                    -- (If there is one, then no whitespace leading chStat is always okay)
                    if not stat.SemicolonList[i-1] then
                        joint(lastChStat:GetLastToken(), chStat:GetFirstToken(), ";")
                    end
                end
            end

            -- A semi-colon is never needed on the last stat in a statlist:
            stat.SemicolonList[#stat.StatementList] = nil

            -- The leading whitespace on the statlist should be stripped
            --[[
			if #stat.StatementList > 0 then
				stript(stat.StatementList[1]:GetFirstToken())
			end
			]]

        elseif stat.Type == 'BreakStat' then
            stript(stat.Token_Break)

        elseif stat.Type == 'ReturnStat' then
            stript(stat.Token_Return)
            for index, expr in pairs(stat.ExprList) do
                stripExpr(expr)
                if stat.Token_CommaList[index] then
                    stript(stat.Token_CommaList[index])
                end
            end
            if #stat.ExprList > 0 then
                joint(stat.Token_Return, stat.ExprList[1]:GetFirstToken())
            end
        elseif stat.Type == 'LocalVarStat' then
            stript(stat.Token_Local)
            for index, var in pairs(stat.VarList) do
                if index == 1 then
                    joint(stat.Token_Local, var)
                else
                    stript(var)
                end
                local comma = stat.Token_VarCommaList[index]
                if comma then
                    stript(comma)
                end
            end
            if stat.Token_Equals then
                stript(stat.Token_Equals)
                for index, expr in pairs(stat.ExprList) do
                    stripExpr(expr)
                    local comma = stat.Token_ExprCommaList[index]
                    if comma then
                        stript(comma)
                    end
                end
            end
        elseif stat.Type == 'LocalFunctionStat' then
            stript(stat.Token_Local)
            joint(stat.Token_Local, stat.FunctionStat.Token_Function)
            joint(stat.FunctionStat.Token_Function, stat.FunctionStat.NameChain[1])
            joint(stat.FunctionStat.NameChain[1], stat.FunctionStat.Token_OpenParen)
            for index, arg in pairs(stat.FunctionStat.ArgList) do
                stript(arg)
                local comma = stat.FunctionStat.Token_ArgCommaList[index]
                if comma then
                    stript(comma)
                end
            end
            stript(stat.FunctionStat.Token_CloseParen)
            bodyjoint(stat.FunctionStat.Token_CloseParen, stat.FunctionStat.Body, stat.FunctionStat.Token_End)
        elseif stat.Type == 'FunctionStat' then
            stript(stat.Token_Function)
            for index, part in pairs(stat.NameChain) do
                if index == 1 then
                    joint(stat.Token_Function, part)
                else
                    stript(part)
                end
                local sep = stat.Token_NameChainSeparator[index]
                if sep then
                    stript(sep)
                end
            end
            stript(stat.Token_OpenParen)
            for index, arg in pairs(stat.ArgList) do
                stript(arg)
                local comma = stat.Token_ArgCommaList[index]
                if comma then
                    stript(comma)
                end
            end
            stript(stat.Token_CloseParen)
            bodyjoint(stat.Token_CloseParen, stat.Body, stat.Token_End)
        elseif stat.Type == 'RepeatStat' then
            stript(stat.Token_Repeat)
            bodyjoint(stat.Token_Repeat, stat.Body, stat.Token_Until)
            stripExpr(stat.Condition)
            joint(stat.Token_Until, stat.Condition:GetFirstToken())
        elseif stat.Type == 'GenericForStat' then
            stript(stat.Token_For)
            for index, var in pairs(stat.VarList) do
                if index == 1 then
                    joint(stat.Token_For, var)
                else
                    stript(var)
                end
                local sep = stat.Token_VarCommaList[index]
                if sep then
                    stript(sep)
                end
            end
            joint(stat.VarList[#stat.VarList], stat.Token_In)
            for index, expr in pairs(stat.GeneratorList) do
                stripExpr(expr)
                if index == 1 then
                    joint(stat.Token_In, expr:GetFirstToken())
                end
                local sep = stat.Token_GeneratorCommaList[index]
                if sep then
                    stript(sep)
                end
            end
            joint(stat.GeneratorList[#stat.GeneratorList]:GetLastToken(), stat.Token_Do)
            bodyjoint(stat.Token_Do, stat.Body, stat.Token_End)
        elseif stat.Type == 'NumericForStat' then
            stript(stat.Token_For)
            for index, var in pairs(stat.VarList) do
                if index == 1 then
                    joint(stat.Token_For, var)
                else
                    stript(var)
                end
                local sep = stat.Token_VarCommaList[index]
                if sep then
                    stript(sep)
                end
            end
            joint(stat.VarList[#stat.VarList], stat.Token_Equals)
            for index, expr in pairs(stat.RangeList) do
                stripExpr(expr)
                if index == 1 then
                    joint(stat.Token_Equals, expr:GetFirstToken())
                end
                local sep = stat.Token_RangeCommaList[index]
                if sep then
                    stript(sep)
                end
            end
            joint(stat.RangeList[#stat.RangeList]:GetLastToken(), stat.Token_Do)
            bodyjoint(stat.Token_Do, stat.Body, stat.Token_End)	
        elseif stat.Type == 'WhileStat' then
            stript(stat.Token_While)
            stripExpr(stat.Condition)
            stript(stat.Token_Do)
            joint(stat.Token_While, stat.Condition:GetFirstToken())
            joint(stat.Condition:GetLastToken(), stat.Token_Do)
            bodyjoint(stat.Token_Do, stat.Body, stat.Token_End)
        elseif stat.Type == 'DoStat' then
            stript(stat.Token_Do)
            stript(stat.Token_End)
            bodyjoint(stat.Token_Do, stat.Body, stat.Token_End)
        elseif stat.Type == 'IfStat' then
            stript(stat.Token_If)
            stripExpr(stat.Condition)
            joint(stat.Token_If, stat.Condition:GetFirstToken())
            joint(stat.Condition:GetLastToken(), stat.Token_Then)
            --
            local lastBodyOpen = stat.Token_Then
            local lastBody = stat.Body
            --
            for _, clause in pairs(stat.ElseClauseList) do
                bodyjoint(lastBodyOpen, lastBody, clause.Token)
                lastBodyOpen = clause.Token
                --
                if clause.Condition then
                    stripExpr(clause.Condition)
                    joint(clause.Token, clause.Condition:GetFirstToken())
                    joint(clause.Condition:GetLastToken(), clause.Token_Then)
                    lastBodyOpen = clause.Token_Then
                end
                stripStat(clause.Body)
                lastBody = clause.Body
            end
            --
            bodyjoint(lastBodyOpen, lastBody, stat.Token_End)

        elseif stat.Type == 'CallExprStat' then
            stripExpr(stat.Expression, true)
            --print("expor;", stat.Expression, "first token:", stat.Expression:GetFirstToken())
            --stat.Expression:GetFirstToken().LeadingWhite = ";"
        elseif stat.Type == 'AssignmentStat' then
            for index, ex in pairs(stat.Lhs) do
                stripExpr(ex)
                local sep = stat.Token_LhsSeparatorList[index]
                if sep then
                    stript(sep)
                end
            end
            stript(stat.Token_Equals)
            for index, ex in pairs(stat.Rhs) do
                stripExpr(ex)
                local sep = stat.Token_RhsSeparatorList[index]
                if sep then
                    stript(sep)
                end
            end
        else
            error("unreachable")
        end
        if stat.Type ~= "StatList" then

        end
    end

    stripStat(ast)

    --dbgprint("finished ast", ast)

    return ast
end
--#endregion

--#region Variable Renaming Functions
--Generate Variable names for MinifyVariables
local indexToVarName do
    local idGen = 0
    local VarDigits = {}
    for i = ('a'):byte(), ('z'):byte() do table.insert(VarDigits, string.char(i)) end
    for i = ('A'):byte(), ('Z'):byte() do table.insert(VarDigits, string.char(i)) end
    for i = ('0'):byte(), ('9'):byte() do table.insert(VarDigits, string.char(i)) end
    table.insert(VarDigits, '_')
    local VarStartDigits = {}
    for i = ('a'):byte(), ('z'):byte() do table.insert(VarStartDigits, string.char(i)) end
    for i = ('A'):byte(), ('Z'):byte() do table.insert(VarStartDigits, string.char(i)) end
    indexToVarName = function (index)
        local id = ''
        local d = index % #VarStartDigits
        index = (index - d) / #VarStartDigits
        id = id..VarStartDigits[d+1]
        while index > 0 do
            local d = index % #VarDigits
            index = (index - d) / #VarDigits
            id = id..VarDigits[d+1]
        end
        return id
    end
end

local function MinifyVariables(globalScope, rootScope)
    -- externalGlobals is a set of global variables that have not been assigned to, that is
    -- global variables defined "externally to the script". We are not going to be renaming 
    -- those, and we have to make sure that we don't collide with them when renaming 
    -- things so we keep track of them in this set.
    local externalGlobals = {}

    -- First we want to rename all of the variables to unique temoraries, so that we can
    -- easily use the scope::GetVar function to check whether renames are valid.
    local temporaryIndex = 0
    for _, var in pairs(globalScope) do
        if var.AssignedTo then
            var:Rename('_TMP_'..temporaryIndex..'_')
            temporaryIndex = temporaryIndex + 1
        else
            -- Not assigned to, external global
            externalGlobals[var.Name] = true
        end
    end
    local function temporaryRename(scope)
        for _, var in pairs(scope.VariableList) do
            if var.Name ~= "..." then
                var:Rename('_TMP_'..temporaryIndex..'_')
                temporaryIndex = temporaryIndex + 1
            end
        end
        for _, childScope in pairs(scope.ChildScopeList) do
            temporaryRename(childScope)
        end
    end

    -- Now we go through renaming, first do globals, we probably want them
    -- to have shorter names in general.
    -- TODO: Rename all vars based on frequency patterns, giving variables
    --       used more shorter names.
    local nextFreeNameIndex = 0
    for _, var in pairs(globalScope) do
        if var.AssignedTo then
            local varName = ''
            repeat
                varName = indexToVarName(nextFreeNameIndex)
                nextFreeNameIndex = nextFreeNameIndex + 1
            until not Keywords[varName] and not externalGlobals[varName]
            var:Rename(varName)
        end
    end

    -- Now rename all local vars
    rootScope.FirstFreeName = nextFreeNameIndex
    local function doRenameScope(scope)
        for _, var in pairs(scope.VariableList) do
            if var.Name ~= "..." then
                local varName = ''
                repeat
                    varName = indexToVarName(scope.FirstFreeName)
                    scope.FirstFreeName = scope.FirstFreeName + 1
                until not Keywords[varName] and not externalGlobals[varName]
                var:Rename(varName)
            end
        end
        for _, childScope in pairs(scope.ChildScopeList) do
            childScope.FirstFreeName = scope.FirstFreeName
            doRenameScope(childScope)
        end
    end
    doRenameScope(rootScope)
end

local function MinifyVariables_2(globalScope, rootScope)
    -- Variable names and other names that are fixed, that we cannot use
    -- Either these are Lua keywords, or globals that are not assigned to,
    -- that is environmental globals that are assigned elsewhere beyond our 
    -- control.
    local globalUsedNames = {}
    for kw, _ in pairs(Keywords) do
        globalUsedNames[kw] = true
    end

    -- Gather a list of all of the variables that we will rename
    local allVariables = {}
    local allLocalVariables = {}
    do
        -- Add applicable globals
        for _, var in pairs(globalScope) do
            if var.AssignedTo then
                -- We can try to rename this global since it was assigned to
                -- (and thus presumably initialized) in the script we are 
                -- minifying.
                table.insert(allVariables, var)
            else
                -- We can't rename this global, mark it as an unusable name
                -- and don't add it to the nename list
                globalUsedNames[var.Name] = true
            end
        end

        -- Recursively add locals, we can rename all of those
        local function addFrom(scope)
            for _, var in pairs(scope.VariableList) do
                if var.Name ~= "..." then
                    table.insert(allVariables, var)
                    table.insert(allLocalVariables, var)
                end
            end
            for _, childScope in pairs(scope.ChildScopeList) do
                addFrom(childScope)
            end
        end
        addFrom(rootScope)
    end

    -- Add used name arrays to variables
    for _, var in pairs(allVariables) do
        var.UsedNameArray = {}
    end

    -- Sort the least used variables first
    table.sort(allVariables, function(a, b)
        return #a.RenameList < #b.RenameList
    end)

    -- Lazy generator for valid names to rename to
    local nextValidNameIndex = 0
    local varNamesLazy = {}
    local function varIndexToValidVarName(i)
        local name = varNamesLazy[i] 
        if not name then
            repeat
                name = indexToVarName(nextValidNameIndex)
                nextValidNameIndex = nextValidNameIndex + 1
            until not globalUsedNames[name]
            varNamesLazy[i] = name
        end
        return name
    end

    -- For each variable, go to rename it
    for _, var in pairs(allVariables) do
        -- Lazy... todo: Make theis pair a proper for-each-pair-like set of loops 
        -- rather than using a renamed flag.
        var.Renamed = true

        -- Find the first unused name
        local i = 1
        while var.UsedNameArray[i] do
            i = i + 1
        end

        -- Rename the variable to that name
        var:Rename(varIndexToValidVarName(i))

        if var.Scope then
            -- Now we need to mark the name as unusable by any variables:
            --  1) At the same depth that overlap lifetime with this one
            --  2) At a deeper level, which have a reference to this variable in their lifetimes
            --  3) At a shallower level, which are referenced during this variable's lifetime
            for _, otherVar in pairs(allVariables) do
                if not otherVar.Renamed then
                    if not otherVar.Scope or otherVar.Scope.Depth < var.Scope.Depth then
                        -- Check Global variable (Which is always at a shallower level)
                        --  or
                        -- Check case 3
                        -- The other var is at a shallower depth, is there a reference to it
                        -- durring this variable's lifetime?
                        for _, refAt in pairs(otherVar.ReferenceLocationList) do
                            if refAt >= var.BeginLocation and refAt <= var.ScopeEndLocation then
                                -- Collide
                                otherVar.UsedNameArray[i] = true
                                break
                            end
                        end

                    elseif otherVar.Scope.Depth > var.Scope.Depth then
                        -- Check Case 2
                        -- The other var is at a greater depth, see if any of the references
                        -- to this variable are in the other var's lifetime.
                        for _, refAt in pairs(var.ReferenceLocationList) do
                            if refAt >= otherVar.BeginLocation and refAt <= otherVar.ScopeEndLocation then
                                -- Collide
                                otherVar.UsedNameArray[i] = true
                                break
                            end
                        end

                    else --otherVar.Scope.Depth must be equal to var.Scope.Depth
                        -- Check case 1
                        -- The two locals are in the same scope
                        -- Just check if the usage lifetimes overlap within that scope. That is, we
                        -- can shadow a local variable within the same scope as long as the usages
                        -- of the two locals do not overlap.
                        if var.BeginLocation < otherVar.EndLocation and
                            var.EndLocation > otherVar.BeginLocation
                        then
                            otherVar.UsedNameArray[i] = true
                        end
                    end
                end
            end
        else
            -- This is a global var, all other globals can't collide with it, and
            -- any local variable with a reference to this global in it's lifetime
            -- can't collide with it.
            for _, otherVar in pairs(allVariables) do
                if not otherVar.Renamed then
                    if otherVar.Type == 'Global' then
                        otherVar.UsedNameArray[i] = true
                    elseif otherVar.Type == 'Local' then
                        -- Other var is a local, see if there is a reference to this global within
                        -- that local's lifetime.
                        for _, refAt in pairs(var.ReferenceLocationList) do
                            if refAt >= otherVar.BeginLocation and refAt <= otherVar.ScopeEndLocation then
                                -- Collide
                                otherVar.UsedNameArray[i] = true
                                break
                            end
                        end
                    else
                        error("unreachable")
                    end
                end
            end
        end
    end

    -- -- 
    -- print("Total Variables: "..#allVariables)
    -- print("Total Range: "..rootScope.BeginLocation.."-"..rootScope.EndLocation)
    -- print("")
    -- for _, var in pairs(allVariables) do
    -- 	io.write("`"..var.Name.."':\n\t#symbols: "..#var.RenameList..
    -- 		"\n\tassigned to: "..tostring(var.AssignedTo))
    -- 	if var.Type == 'Local' then
    -- 		io.write("\n\trange: "..var.BeginLocation.."-"..var.EndLocation)
    -- 		io.write("\n\tlocal type: "..var.Info.Type)
    -- 	end
    -- 	io.write("\n\n")
    -- end

    -- -- First we want to rename all of the variables to unique temoraries, so that we can
    -- -- easily use the scope::GetVar function to check whether renames are valid.
    -- local temporaryIndex = 0
    -- for _, var in pairs(allVariables) do
    -- 	var:Rename('_TMP_'..temporaryIndex..'_')
    -- 	temporaryIndex = temporaryIndex + 1
    -- end

    -- For each variable, we need to build a list of names that collide with it

    --
    --error()
end

local function BeautifyVariables(globalScope, rootScope)
    local externalGlobals = {}
    for _, var in pairs(globalScope) do
        if not var.AssignedTo then
            externalGlobals[var.Name] = true
        end
    end

    local localNumber = 1
    local globalNumber = 1

    local function setVarName(var, name)
        var.Name = name
        for _, setter in pairs(var.RenameList) do
            setter(name)
        end
    end

    for _, var in pairs(globalScope) do
        if var.AssignedTo then
            setVarName(var, 'G_'..globalNumber)
            globalNumber = globalNumber + 1
        end
    end


    local function modify(scope)
        for _, var in pairs(scope.VariableList) do
            if var.Name ~= "..." then
                local name = 'L_'..localNumber..'_'
                if var.Info2.Type == 'Argument' then
                    name = name..'arg'..var.Info2.Index
                elseif var.Info2.Type == 'LocalFunction' then
                    name = name..'func'
                elseif var.Info2.Type == 'ForRange' then
                    name = name..'forvar'..var.Info2.Index
                end
                setVarName(var, name)
                localNumber = localNumber + 1
            end
        end
        for _, scope in pairs(scope.ChildScopeList) do
            modify(scope)
        end
    end
    modify(rootScope)
end
--#endregion

--#region Module Interface Functions

-- note: minify2 is glitchy, very.
-- minify(sourcecode: string, useminify2: boolean)
local function Minify(src, useminify2)
    local ast = CreateLuaParser(src)
    local global_scope, root_scope = AddVariableInfo(ast)
    if useminify2 then
        MinifyVariables_2(global_scope, root_scope)
    else
        MinifyVariables(global_scope, root_scope)
    end
    local result = StripAst(ast)
    return StringAst(result)
end

-- beautify(sourcecode: string, beautifyvariables: boolean?, solvemath: boolean?, solveconstants: boolean?, solveifstats: boolean?, replaceconstants: boolean?)
local function Beautify(src, beautifyvariables, solvemath, ...)
    local ast = CreateLuaParser(src)
    if beautifyvariables or solvemath then --Variable info required for solvemath and beautifyvariables
        local global_scope, root_scope = AddVariableInfo(ast)
        if beautifyvariables then
            BeautifyVariables(global_scope, root_scope)
        end
        if solvemath then
            SolveMath(ast, ...)
        end
    end
    local result = FormatAst(ast) --solveconstants: boolean?, solveifstats: boolean?, replaceconstants: boolean?
    return StringAst(result)
end

--#endregion

return {
    Beautify = Beautify,
    Minify = Minify
}