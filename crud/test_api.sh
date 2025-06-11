#!/bin/bash

BASE_URL="http://localhost:8080"

echo "Testing Cookie CRUD API..."

# Test POST - Create cookie
echo "1. Creating cookie..."
curl -X POST $BASE_URL/cookies \
  -H "Content-Type: application/json" \
  -d '{"cookie": "session123", "user_id": 1001}' \
  -w "\nStatus: %{http_code}\n\n"

# Test GET - Get all cookies
echo "2. Getting all cookies..."
curl -X GET $BASE_URL/cookies \
  -w "\nStatus: %{http_code}\n\n"

# Test GET - Get specific cookie
echo "3. Getting specific cookie..."
curl -X GET $BASE_URL/cookies/session123 \
  -w "\nStatus: %{http_code}\n\n"

# Test PUT - Update cookie
echo "4. Updating cookie..."
curl -X PUT $BASE_URL/cookies/session123 \
  -H "Content-Type: application/json" \
  -d '{"user_id": 1002, "extra_data": "updated"}' \
  -w "\nStatus: %{http_code}\n\n"

# Test GET - Verify update
echo "5. Verifying update..."
curl -X GET $BASE_URL/cookies/session123 \
  -w "\nStatus: %{http_code}\n\n"

# Test DELETE - Delete cookie
echo "6. Deleting cookie..."
curl -X DELETE $BASE_URL/cookies/session123 \
  -w "\nStatus: %{http_code}\n\n"

# Test GET - Verify deletion
echo "7. Verifying deletion..."
curl -X GET $BASE_URL/cookies/session123 \
  -w "\nStatus: %{http_code}\n\n"

echo "API testing completed!"