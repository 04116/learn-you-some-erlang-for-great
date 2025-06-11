-- wrk Lua script for mixed workload
-- This script performs a mix of GET, POST, PUT, and DELETE operations

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

-- Array of existing cookie IDs for GET/PUT/DELETE operations
local existing_cookies = {
    "session123", "user456", "admin789", "test001", "demo999",
    "bench001", "bench002", "bench003", "bench004", "bench005"
}

-- Counter for unique cookies
local counter = 0

-- Setup function called once per thread
function setup(thread)
    thread:set("id", counter)
    counter = counter + 1
end

-- Request function called for each request
function request()
    local thread_id = id or 0
    local operation = math.random(1, 10)
    
    if operation <= 4 then
        -- 40% GET requests (read all cookies)
        return wrk.format("GET", "/cookies")
        
    elseif operation <= 6 then
        -- 20% GET specific cookie
        local cookie_id = existing_cookies[math.random(1, #existing_cookies)]
        return wrk.format("GET", "/cookies/" .. cookie_id)
        
    elseif operation <= 8 then
        -- 20% POST (create new cookie)
        local cookie_name = "bench_" .. thread_id .. "_" .. math.random(1, 10000)
        local user_id = math.random(1000, 9999)
        local body = string.format('{"cookie": "%s", "user_id": %d}', cookie_name, user_id)
        
        return wrk.format("POST", "/cookies", {
            ["Content-Type"] = "application/json",
            ["Content-Length"] = #body
        }, body)
        
    elseif operation <= 9 then
        -- 10% PUT (update existing cookie)
        local cookie_id = existing_cookies[math.random(1, #existing_cookies)]
        local new_user_id = math.random(1000, 9999)
        local body = string.format('{"user_id": %d, "updated": true}', new_user_id)
        
        return wrk.format("PUT", "/cookies/" .. cookie_id, {
            ["Content-Type"] = "application/json",
            ["Content-Length"] = #body
        }, body)
        
    else
        -- 10% DELETE
        local cookie_id = "bench_" .. thread_id .. "_" .. math.random(1, 1000)
        return wrk.format("DELETE", "/cookies/" .. cookie_id)
    end
end

-- Response function called for each response
function response(status, headers, body)
    -- Log only errors
    if status >= 400 and status ~= 404 and status ~= 409 then
        print("Error: HTTP " .. status .. " - " .. body)
    end
end