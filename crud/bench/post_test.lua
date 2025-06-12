-- wrk performance test script for POST /cookies endpoint
-- Usage: wrk -t2 -c5 -d10s -s bench/post_test.lua http://localhost:8080/cookies

-- Request counter for unique cookie names
local counter = 0
local thread_id = 0

-- Initialize thread-specific data
function setup(thread)
    thread:set("id", thread_id)
    thread_id = thread_id + 1
end

-- Generate unique request body for each request
function request()
    counter = counter + 1
    local cookie_name = "perf_test_" .. thread_id .. "_" .. counter
    local user_id = 1000 + (counter % 9000)  -- Random user ID between 1000-9999
    
    local body = string.format([[{
        "cookie": "%s",
        "user_id": %d,
        "test_type": "performance",
        "timestamp": %d,
        "thread_id": %d,
        "request_num": %d
    }]], cookie_name, user_id, os.time(), thread_id, counter)
    
    return wrk.format("POST", nil, {
        ["Content-Type"] = "application/json",
        ["Content-Length"] = string.len(body)
    }, body)
end

-- Process response and collect statistics
function response(status, headers, body)
    if status ~= 201 then
        print("Unexpected status: " .. status)
        print("Response body: " .. body)
    end
end

-- Print final statistics
function done(summary, latency, requests)
    io.write("------------------------------\n")
    io.write("Performance Test Results\n")
    io.write("------------------------------\n")
    io.write(string.format("Total Requests: %d\n", summary.requests))
    io.write(string.format("Total Errors: %d\n", summary.errors.connect + summary.errors.read + summary.errors.write + summary.errors.status + summary.errors.timeout))
    io.write(string.format("Requests/sec: %.2f\n", summary.requests / (summary.duration / 1000000)))
    io.write(string.format("Transfer/sec: %.2f KB\n", summary.bytes / (summary.duration / 1000000) / 1024))
    io.write(string.format("Avg Latency: %.2f ms\n", latency.mean / 1000))
    io.write(string.format("Max Latency: %.2f ms\n", latency.max / 1000))
    io.write(string.format("90th Percentile: %.2f ms\n", latency:percentile(90) / 1000))
    io.write(string.format("99th Percentile: %.2f ms\n", latency:percentile(99) / 1000))
end