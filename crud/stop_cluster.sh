#!/bin/bash

# =============================================================================
# Cookie CRUD Cluster Shutdown Script
# =============================================================================
# This script stops all Cookie CRUD cluster instances gracefully.
# =============================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Stopping Cookie CRUD Cluster...${NC}"

# Function to stop a node
stop_node() {
    local node_name=$1
    local port=$2
    
    echo -e "${YELLOW}Stopping node: ${node_name}${NC}"
    
    # Try to stop the node gracefully using Erlang RPC
    erl -name "stopper@localhost" \
        -setcookie cookie_crud_cluster \
        -hidden \
        -eval "rpc:call('${node_name}@localhost', init, stop, []), halt()." \
        -noshell 2>/dev/null || true
    
    # Give the node time to shut down
    sleep 1
    
    # Check if the node is still running
    if curl -s "http://localhost:${port}/cache" >/dev/null 2>&1; then
        echo -e "${RED}Warning: Node ${node_name} may still be running${NC}"
    else
        echo -e "${GREEN}Node ${node_name} stopped successfully${NC}"
    fi
}

# Stop all cluster nodes
stop_node "cookie_crud_node1" "8081"
stop_node "cookie_crud_node2" "8082"
stop_node "cookie_crud_node3" "8083"

# Clean up any remaining Erlang processes
echo -e "${YELLOW}Cleaning up any remaining processes...${NC}"
pkill -f "cookie_crud_node" 2>/dev/null || true

echo -e "${GREEN}Cluster shutdown complete!${NC}"