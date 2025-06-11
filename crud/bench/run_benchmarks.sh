#!/bin/bash

# Cookie CRUD API Benchmark Suite
# Requires wrk to be installed: brew install wrk

set -e

BASE_URL="http://localhost:8080"
RESULTS_DIR="bench/results"
SCRIPTS_DIR="bench/wrk_scripts"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default benchmark parameters
DURATION="30s"
THREADS="4"
CONNECTIONS="100"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--duration)
            DURATION="$2"
            shift 2
            ;;
        -t|--threads)
            THREADS="$2"
            shift 2
            ;;
        -c|--connections)
            CONNECTIONS="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  -d, --duration DURATION     Test duration (default: 30s)"
            echo "  -t, --threads THREADS       Number of threads (default: 4)"
            echo "  -c, --connections CONNS     Number of connections (default: 100)"
            echo "  -h, --help                  Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option $1"
            exit 1
            ;;
    esac
done

print_header() {
    echo -e "${BLUE}============================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}============================================${NC}"
}

print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_dependencies() {
    print_info "Checking dependencies..."
    
    if ! command -v wrk &> /dev/null; then
        print_error "wrk is not installed. Please install it with: brew install wrk"
        exit 1
    fi
    
    if ! command -v curl &> /dev/null; then
        print_error "curl is not installed"
        exit 1
    fi
    
    print_info "All dependencies are available"
}

check_server() {
    print_info "Checking if server is running on $BASE_URL..."
    
    if curl -sf "$BASE_URL/cookies" > /dev/null 2>&1; then
        print_info "Server is running"
    else
        print_error "Server is not running on $BASE_URL"
        print_info "Please start the server with: make dev"
        exit 1
    fi
}

prepare_environment() {
    print_info "Preparing benchmark environment..."
    
    # Create results directory
    mkdir -p "$RESULTS_DIR"
    
    # Create some initial test data
    print_info "Creating initial test data..."
    
    test_cookies=(
        '{"cookie": "session123", "user_id": 1001}'
        '{"cookie": "user456", "user_id": 1002}'
        '{"cookie": "admin789", "user_id": 1003}'
        '{"cookie": "test001", "user_id": 1004}'
        '{"cookie": "demo999", "user_id": 1005}'
        '{"cookie": "bench001", "user_id": 2001}'
        '{"cookie": "bench002", "user_id": 2002}'
        '{"cookie": "bench003", "user_id": 2003}'
        '{"cookie": "bench004", "user_id": 2004}'
        '{"cookie": "bench005", "user_id": 2005}'
    )
    
    for cookie_data in "${test_cookies[@]}"; do
        curl -sf -X POST "$BASE_URL/cookies" \
             -H "Content-Type: application/json" \
             -d "$cookie_data" > /dev/null 2>&1 || true
    done
    
    print_info "Initial test data created"
}

run_benchmark() {
    local name="$1"
    local script="$2"
    local description="$3"
    
    print_header "$name"
    print_info "$description"
    print_info "Duration: $DURATION, Threads: $THREADS, Connections: $CONNECTIONS"
    
    local timestamp=$(date +"%Y%m%d_%H%M%S")
    local result_file="$RESULTS_DIR/${name,,}_${timestamp}.txt"
    
    echo "Benchmark: $name" > "$result_file"
    echo "Description: $description" >> "$result_file"
    echo "Timestamp: $(date)" >> "$result_file"
    echo "Duration: $DURATION" >> "$result_file"
    echo "Threads: $THREADS" >> "$result_file"
    echo "Connections: $CONNECTIONS" >> "$result_file"
    echo "Script: $script" >> "$result_file"
    echo "----------------------------------------" >> "$result_file"
    
    if [[ -f "$script" ]]; then
        wrk -t"$THREADS" -c"$CONNECTIONS" -d"$DURATION" -s "$script" "$BASE_URL" | tee -a "$result_file"
    else
        wrk -t"$THREADS" -c"$CONNECTIONS" -d"$DURATION" "$BASE_URL$script" | tee -a "$result_file"
    fi
    
    echo "" >> "$result_file"
    print_info "Results saved to: $result_file"
    echo ""
}

run_all_benchmarks() {
    print_header "Cookie CRUD API Benchmark Suite"
    
    # 1. GET /cookies (read all)
    run_benchmark "GET_ALL_COOKIES" "/cookies" \
                  "Benchmark reading all cookies"
    
    # 2. POST /cookies (create)
    run_benchmark "POST_CREATE_COOKIE" "$SCRIPTS_DIR/post_cookie.lua" \
                  "Benchmark creating new cookies with random data"
    
    # 3. GET /cookies (after creates)
    run_benchmark "GET_ALL_AFTER_CREATES" "/cookies" \
                  "Benchmark reading all cookies after creating many"
    
    # 4. Mixed workload
    run_benchmark "MIXED_WORKLOAD" "$SCRIPTS_DIR/mixed_workload.lua" \
                  "Benchmark mixed CRUD operations (40% GET all, 20% GET one, 20% POST, 10% PUT, 10% DELETE)"
    
    # 5. GET specific cookies
    run_benchmark "GET_SPECIFIC_COOKIES" "$SCRIPTS_DIR/get_cookies.lua" \
                  "Benchmark reading all cookies repeatedly"
}

generate_summary() {
    print_header "Benchmark Summary"
    
    local summary_file="$RESULTS_DIR/summary_$(date +"%Y%m%d_%H%M%S").txt"
    
    echo "Cookie CRUD API Benchmark Summary" > "$summary_file"
    echo "Generated: $(date)" >> "$summary_file"
    echo "Parameters: Duration=$DURATION, Threads=$THREADS, Connections=$CONNECTIONS" >> "$summary_file"
    echo "========================================" >> "$summary_file"
    echo "" >> "$summary_file"
    
    # Extract key metrics from the latest results
    for result_file in "$RESULTS_DIR"/*.txt; do
        if [[ -f "$result_file" && "$result_file" != "$summary_file" ]]; then
            echo "File: $(basename "$result_file")" >> "$summary_file"
            
            # Extract requests/sec and latency
            grep -E "(Requests/sec|Latency)" "$result_file" >> "$summary_file" 2>/dev/null || true
            echo "" >> "$summary_file"
        fi
    done
    
    print_info "Summary saved to: $summary_file"
    
    # Display quick summary
    echo ""
    print_info "Quick Summary (Requests/sec):"
    for result_file in "$RESULTS_DIR"/*.txt; do
        if [[ -f "$result_file" && "$result_file" != "$summary_file" ]]; then
            local req_per_sec=$(grep "Requests/sec:" "$result_file" 2>/dev/null | awk '{print $2}' | head -1)
            if [[ -n "$req_per_sec" ]]; then
                printf "  %-25s %s req/s\n" "$(basename "$result_file" .txt):" "$req_per_sec"
            fi
        fi
    done
}

main() {
    check_dependencies
    check_server
    prepare_environment
    run_all_benchmarks
    generate_summary
    
    print_header "Benchmark Complete"
    print_info "All benchmark results are saved in: $RESULTS_DIR"
    print_info "To run individual benchmarks, use the wrk scripts in: $SCRIPTS_DIR"
}

# Run main function
main "$@"