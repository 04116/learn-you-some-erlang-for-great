#!/bin/bash

# Quick benchmark script for Cookie CRUD API
# Runs a short 10-second test to verify performance

set -e

BASE_URL="http://localhost:8080"

echo "🚀 Quick Performance Test"
echo "========================="

# Check if server is running
if ! curl -sf "$BASE_URL/cookies" > /dev/null 2>&1; then
    echo "❌ Server is not running on $BASE_URL"
    echo "Please start the server with: make dev"
    exit 1
fi

echo "✅ Server is running"

# Check if wrk is installed
if ! command -v wrk &> /dev/null; then
    echo "❌ wrk is not installed"
    echo "Install with: brew install wrk"
    exit 1
fi

echo "✅ wrk is available"
echo ""

# Quick GET test
echo "📊 Testing GET /cookies (10 seconds)..."
wrk -t2 -c10 -d10s "$BASE_URL/cookies"

echo ""
echo "📊 Testing POST /cookies (10 seconds)..."
wrk -t2 -c10 -d10s -s bench/wrk_scripts/post_cookie.lua "$BASE_URL"

echo ""
echo "✅ Quick benchmark complete!"
echo "For comprehensive benchmarks, run: ./bench/run_benchmarks.sh"