# Cookie CRUD API

A RESTful API server built with Erlang, Cowboy, and SQLite for managing cookie data.

## Features

- **RESTful API** with full CRUD operations
- **High-performance SQLite** with WAL mode, connection pooling, and optimizations
- **JSON storage** with generated columns and strategic indexing
- **Type specifications** with Dialyzer static analysis and eqWAlizer support
- **Production-ready** with OTP application structure and performance monitoring
- **Comprehensive testing** with API test suite and wrk benchmarking

## Project Structure

```
├── src/                    # Source code
│   ├── cookie_crud.erl    # HTTP handler and database logic
│   ├── cookie_crud_app.erl # OTP application callback
│   ├── cookie_crud_sup.erl # OTP supervisor
│   └── cookie_crud.app.src # Application resource file
├── config/                # Configuration files
│   ├── sys.config        # System configuration
│   └── vm.args          # VM arguments
├── rebar.config          # Rebar3 build configuration
├── Makefile             # Build automation
└── test_api.sh          # API test script
```

## Requirements

- **Erlang/OTP 24+**
- **rebar3**
- **Make** (optional, for convenience targets)

## Quick Start

### Build and Dependencies

```bash
# Fetch dependencies and compile
rebar3 compile

# Or using make
make compile
```

### Development

```bash
# Start development shell with hot reloading
rebar3 shell

# Or using make
make dev
```

### Testing

```bash
# Run API tests (requires server to be running)
./test_api.sh

# Or using make
make test
```

## Available Make Targets

```bash
make compile      # Compile source code
make deps         # Fetch dependencies  
make shell        # Start Erlang shell with project
make dev          # Start development environment
make test         # Run API tests
make dialyzer     # Run Dialyzer static analysis
make xref         # Run cross reference analysis
make bench        # Run comprehensive performance benchmarks
make quick-bench  # Run quick performance test
make clean        # Clean build files
make clean-db     # Remove database file
make clean-all    # Clean everything including PLT files
make release      # Build production release
make tar          # Create release tarball
make help         # Show available targets
```

## API Endpoints

### Base URL
`http://localhost:8080`

### Endpoints

| Method | Path              | Description           |
|--------|-------------------|-----------------------|
| GET    | `/cookies`        | List all cookies      |
| GET    | `/cookies/:id`    | Get specific cookie   |
| POST   | `/cookies`        | Create new cookie     |
| PUT    | `/cookies/:id`    | Update cookie         |
| DELETE | `/cookies/:id`    | Delete cookie         |

### Request/Response Format

All requests and responses use JSON format.

#### Create Cookie (POST /cookies)
```json
{
  "cookie": "session123",
  "user_id": 1001
}
```

Response (201 Created):
```json
{
  "cookie": "session123",
  "user_id": 1001,
  "created": "1749658366",
  "last_used": "1749658366"
}
```

#### Get All Cookies (GET /cookies)
Response (200 OK):
```json
{
  "cookies": [
    {
      "cookie": "session123",
      "user_id": 1001,
      "created": "1749658366",
      "last_used": "1749658366"
    }
  ]
}
```

#### Update Cookie (PUT /cookies/:id)
```json
{
  "user_id": 1002,
  "extra_data": "updated"
}
```

Response (200 OK):
```json
{
  "cookie": "session123",
  "user_id": 1002,
  "created": "1749658366",
  "last_used": "1749658367",
  "extra_data": "updated"
}
```

#### Error Responses
```json
{
  "error": "Cookie not found"
}
```

## Database Schema

The application uses SQLite with a single table:

```sql
CREATE TABLE Cookie (
  Cookie   TEXT    NOT NULL AS (json_extract(Data, '$.cookie'))  STORED UNIQUE,
  UserID   INTEGER NOT NULL AS (json_extract(Data, '$.user_id')) STORED,
  Created  INTEGER NOT NULL AS (json_extract(Data, '$.created')) STORED,
  LastUsed INTEGER AS (json_extract(Data, '$.last_used')) CHECK (LastUsed>0),
  Data     TEXT    NOT NULL
);
```

- **Generated columns** extract JSON fields for indexing and constraints
- **UNIQUE constraint** on Cookie field prevents duplicates
- **JSON storage** allows flexible data structure

## Performance Optimizations

This API is optimized for high-performance server workloads:

### SQLite Optimizations
- **WAL Mode**: Write-Ahead Logging for better concurrency
- **Connection Pooling**: 10 reusable database connections
- **Optimized Pragmas**: Tuned for server workloads
  - 64MB cache size
  - Memory-mapped I/O (128MB)
  - Normal synchronous mode
  - 5-second busy timeout

### Database Features
- **Strategic Indexing**: UserID, Created, LastUsed columns
- **Generated Columns**: Extracted JSON fields for fast queries
- **JSON Storage**: Flexible schema with typed access

### Benchmarking
```bash
# Quick performance test (10 seconds)
make quick-bench

# Comprehensive benchmark suite (30 seconds each)
make bench

# Custom benchmark
./bench/run_benchmarks.sh -d 60s -t 8 -c 200
```

**Typical Performance**:
- **5,000+ requests/sec** for read operations
- **2,000+ requests/sec** for write operations
- **Sub-10ms latency** for most operations
- Handles **100+ concurrent connections**

See [PERFORMANCE.md](PERFORMANCE.md) for detailed optimization guide.

## Configuration

### Application Configuration (config/sys.config)
```erlang
[
 {cookie_crud, [
     {port, 8080},
     {db_file, "cookies.db"}
 ]}
].
```

### VM Arguments (config/vm.args)
```
-name cookie_crud@127.0.0.1
-setcookie cookie_crud_cookie
-heart
+K true
+A30
```

## Static Analysis

The project includes comprehensive static analysis tools:

### Dialyzer
```bash
make dialyzer
```
- **Type checking** with comprehensive type specifications
- **Dead code detection**
- **Unreachable code analysis**
- **Pattern matching validation**

### Cross Reference Analysis
```bash
make xref
```
- **Undefined function detection**
- **Unused function analysis**
- **Dependency verification**

## Production Deployment

### Build Release
```bash
make release
```

### Create Distribution Package
```bash
make tar
```

### Start Production Release
```bash
_build/prod/rel/cookie_crud/bin/cookie_crud start
```

## Development Tips

1. **Hot Reloading**: Use `rebar3 shell` for development with automatic code reloading
2. **Type Safety**: Add type specs to all functions for better Dialyzer analysis
3. **Error Handling**: Always match return values from database operations
4. **Testing**: Run API tests after any changes to ensure functionality

## Dependencies

- **cowboy**: HTTP server framework
- **esqlite**: SQLite NIF driver
- **jsx**: JSON encoder/decoder

## License

Apache 2.0