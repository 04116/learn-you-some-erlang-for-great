#!/bin/bash

# =============================================================================
# Cookie CRUD Cluster Test Script
# =============================================================================
# This script tests the cluster functionality including cache synchronization,
# load balancing, and node health monitoring.
# =============================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing Cookie CRUD Cluster...${NC}"

# Cluster endpoints
NODES=("http://localhost:8081" "http://localhost:8082" "http://localhost:8083")

# Test counter
TESTS_PASSED=0
TESTS_TOTAL=0

# Function to run a test
run_test() {
    local test_name=$1
    local test_command=$2
    
    echo -e "${YELLOW}Running test: ${test_name}${NC}"
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    
    if eval "$test_command"; then
        echo -e "${GREEN}✓ PASSED: ${test_name}${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗ FAILED: ${test_name}${NC}"
    fi
    echo ""
}

# Function to check if all nodes are healthy
check_cluster_health() {
    echo -e "${BLUE}Checking cluster health...${NC}"
    
    for node in "${NODES[@]}"; do
        if curl -s "${node}/cache" >/dev/null 2>&1; then
            echo -e "${GREEN}✓ Node ${node} is healthy${NC}"
        else
            echo -e "${RED}✗ Node ${node} is not responding${NC}"
            return 1
        fi
    done
    return 0
}

# Test 1: Cluster Health Check
run_test "Cluster Health Check" "check_cluster_health"

# Test 2: Cache Status Endpoint
run_test "Cache Status Endpoint" "curl -s ${NODES[0]}/cache | jq -e '.cache_stats' >/dev/null"

# Test 3: Create Cookie on Node 1
run_test "Create Cookie on Node 1" "curl -s -X POST ${NODES[0]}/cookies -H 'Content-Type: application/json' -d '{\"cookie\":\"cluster_test_cookie\",\"user_id\":1234}' | jq -e '.cookie' >/dev/null"

# Test 4: Verify Cookie Exists on All Nodes (Cache Sync)
run_test "Verify Cookie on Node 2" "curl -s ${NODES[1]}/cookies/cluster_test_cookie | jq -e '.cookie' >/dev/null"
run_test "Verify Cookie on Node 3" "curl -s ${NODES[2]}/cookies/cluster_test_cookie | jq -e '.cookie' >/dev/null"

# Test 5: Update Cookie on Node 2
run_test "Update Cookie on Node 2" "curl -s -X PUT ${NODES[1]}/cookies/cluster_test_cookie -H 'Content-Type: application/json' -d '{\"description\":\"Updated from node 2\"}' | jq -e '.last_used' >/dev/null"

# Test 6: Verify Update Propagated to Other Nodes
run_test "Verify Update on Node 1" "curl -s ${NODES[0]}/cookies/cluster_test_cookie | jq -e '.description' >/dev/null"
run_test "Verify Update on Node 3" "curl -s ${NODES[2]}/cookies/cluster_test_cookie | jq -e '.description' >/dev/null"

# Test 7: Cache Performance Test
run_test "Cache Performance Test" "
    # First request (cache miss)
    time1=\$(curl -s -w '%{time_total}' ${NODES[0]}/cookies >/dev/null)
    # Second request (cache hit)  
    time2=\$(curl -s -w '%{time_total}' ${NODES[0]}/cookies >/dev/null)
    # Cache hit should be faster
    echo \"First request: \${time1}s, Second request: \${time2}s\"
    [[ \$(echo \"\$time2 < \$time1\" | bc -l) -eq 1 ]]
"

# Test 8: Load Balancing Test
run_test "Load Balancing Test" "
    # Send requests to different nodes and verify responses
    for node in ${NODES[@]}; do
        curl -s \${node}/cookies >/dev/null || exit 1
    done
"

# Test 9: Cache Invalidation Test
run_test "Cache Invalidation Test" "
    # Create a cookie
    curl -s -X POST ${NODES[0]}/cookies -H 'Content-Type: application/json' -d '{\"cookie\":\"invalidation_test\",\"user_id\":5678}' >/dev/null
    # Verify it's cached
    curl -s ${NODES[1]}/cookies/invalidation_test >/dev/null
    # Delete the cookie
    curl -s -X DELETE ${NODES[2]}/cookies/invalidation_test >/dev/null
    # Verify cache is invalidated (should return 404)
    ! curl -s ${NODES[0]}/cookies/invalidation_test | jq -e '.cookie' >/dev/null
"

# Test 10: Metrics Collection
run_test "Metrics Collection" "curl -s ${NODES[0]}/metrics | grep -q 'http_requests_total'"

# Test 11: Cache Statistics
run_test "Cache Statistics" "curl -s ${NODES[0]}/cache | jq -e '.cache_stats.hits' >/dev/null"

# Test 12: Cluster Status
run_test "Cluster Status" "curl -s ${NODES[0]}/cache | jq -e '.cluster_status.current_node' >/dev/null"

# Display results
echo -e "${BLUE}=== Test Results ===${NC}"
echo -e "Tests Passed: ${GREEN}${TESTS_PASSED}${NC}"
echo -e "Tests Failed: ${RED}$((TESTS_TOTAL - TESTS_PASSED))${NC}"
echo -e "Total Tests: ${TESTS_TOTAL}"

if [ $TESTS_PASSED -eq $TESTS_TOTAL ]; then
    echo -e "${GREEN}🎉 All tests passed! Cluster is working correctly.${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed. Check the cluster configuration.${NC}"
    exit 1
fi