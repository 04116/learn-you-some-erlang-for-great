-- wrk Lua script for GET /cookies
-- This script reads all cookies

-- Request function called for each request
function request()
    return wrk.format("GET", "/cookies")
end

-- Response function called for each response
function response(status, headers, body)
    if status ~= 200 then
        print("Error: HTTP " .. status .. " - " .. body)
    end
end