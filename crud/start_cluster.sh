#!/bin/bash

# =============================================================================
# Cookie CRUD Cluster Startup Script
# =============================================================================
# This script starts multiple Cookie CRUD instances as a cluster.
# Each instance runs on a different port and connects to the others.
# =============================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Starting Cookie CRUD Cluster...${NC}"

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 is not installed or not in PATH${NC}"
    exit 1
fi

# Compile the project first
echo -e "${YELLOW}Compiling project...${NC}"
make compile

# Function to start a node
start_node() {
    local node_name=$1
    local config_file=$2
    local port=$3
    
    echo -e "${GREEN}Starting node: ${node_name} on port ${port}${NC}"
    
    # Start the node in the background using erl directly
    erl -pa _build/default/lib/*/ebin \
        -config "config/${config_file}" \
        -name "${node_name}@localhost" \
        -setcookie cookie_crud_cluster \
        -detached \
        -eval "application:ensure_all_started(cookie_crud)"
    
    # Give the node time to start
    sleep 2
    
    # Check if the node is responding
    if curl -s "http://localhost:${port}/cache" >/dev/null 2>&1; then
        echo -e "${GREEN}Node ${node_name} started successfully on port ${port}${NC}"
    else
        echo -e "${RED}Warning: Node ${node_name} may not have started correctly${NC}"
    fi
}

# Start the cluster nodes
start_node "cookie_crud_node1" "node1" "8081"
start_node "cookie_crud_node2" "node2" "8082"
start_node "cookie_crud_node3" "node3" "8083"

echo -e "${BLUE}Cluster startup complete!${NC}"
echo -e "${YELLOW}Cluster endpoints:${NC}"
echo -e "  Node 1: http://localhost:8081"
echo -e "  Node 2: http://localhost:8082"
echo -e "  Node 3: http://localhost:8083"
echo ""
echo -e "${YELLOW}Management endpoints:${NC}"
echo -e "  Cache Status: http://localhost:8081/cache"
echo -e "  Metrics: http://localhost:8081/metrics"
echo ""
echo -e "${YELLOW}To stop the cluster, run: ${GREEN}./stop_cluster.sh${NC}"
echo -e "${YELLOW}To test the cluster, run: ${GREEN}./test_cluster.sh${NC}"