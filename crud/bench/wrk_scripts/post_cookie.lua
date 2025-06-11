-- wrk Lua script for POST /cookies
-- This script creates new cookies with random data

math.randomseed(os.time())

-- Generate random string
function random_string(length)
    local charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    local result = ""
    for i = 1, length do
        local pos = math.random(1, #charset)
        result = result .. charset:sub(pos, pos)
    end
    return result
end

-- Counter for unique cookies
local counter = 0

-- Setup function called once per thread
function setup(thread)
    thread:set("id", counter)
    counter = counter + 1
end

-- Request function called for each request
function request()
    -- Get thread ID
    local thread_id = id or 0
    
    -- Generate unique cookie name
    local cookie_name = "bench_cookie_" .. thread_id .. "_" .. math.random(1, 100000)
    local user_id = math.random(1000, 9999)
    
    -- Create JSON payload
    local body = string.format('{"cookie": "%s", "user_id": %d}', cookie_name, user_id)
    
    -- Return HTTP request
    return wrk.format("POST", "/cookies", {
        ["Content-Type"] = "application/json",
        ["Content-Length"] = #body
    }, body)
end

-- Response function called for each response
function response(status, headers, body)
    if status ~= 201 then
        print("Error: HTTP " .. status .. " - " .. body)
    end
end