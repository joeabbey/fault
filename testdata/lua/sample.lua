-- Sample Lua file for parser testing

-- Require statements (various forms)
local json = require 'cjson'
local socket = require "socket"
local lfs = require('lfs')
local lpeg = require("lpeg")
local utils = require 'app.utils'

-- Module table
local M = {}

-- Module constants
M.VERSION = "1.0.0"
M.MAX_RETRIES = 3

-- Local variables
local DEFAULT_TIMEOUT = 30
local cache = {}

-- Local function
local function validate_input(data)
    if type(data) ~= "table" then
        return false, "expected table"
    end
    return true
end

-- Module function (dot syntax)
function M.initialize(config)
    M.config = config or {}
    M.timeout = M.config.timeout or DEFAULT_TIMEOUT
end

function M.process(items)
    local results = {}
    for _, item in ipairs(items) do
        if validate_input(item) then
            table.insert(results, item)
        end
    end
    return results
end

-- Module method assigned as variable
M.cleanup = function(self)
    cache = {}
    self.config = nil
end

-- Top-level function (global)
function helper_format(str)
    return string.format("[%s]", str)
end

-- Nested module function
function M.db.connect(host, port)
    return socket.connect(host, port)
end

-- Another local function
local function internal_log(level, message)
    print(string.format("[%s] %s", level, message))
end

-- Global function
function create_logger(name)
    return function(msg)
        internal_log("INFO", name .. ": " .. msg)
    end
end

return M
