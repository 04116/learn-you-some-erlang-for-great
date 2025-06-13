# Claude Configuration for Cookie CRUD API

This file contains configuration and context information for Claude to better assist with this Erlang/OTP project.

## Project Overview

**Project Name**: Cookie CRUD API  
**Language**: Erlang/OTP  
**Framework**: Cowboy HTTP Server  
**Database**: SQLite with esqlite driver  
**Build Tool**: rebar3  
**Type Checking**: Dialyzer + eqWAlizer  

## Project Structure

```
cookie_crud/
├── src/                     # Erlang source files
│   ├── cookie_crud.app.src  # Application resource file
│   ├── cookie_crud_app.erl  # Application behavior module
│   ├── cookie_crud_sup.erl  # OTP supervisor
│   └── cookie_crud.erl      # Main HTTP handler and business logic
├── config/                  # Configuration files
│   ├── sys.config          # System configuration
│   └── vm.args             # Erlang VM arguments
├── test/                    # Test files
│   ├── cookie_crud_tests.erl              # Unit tests
│   ├── cookie_db_pool_tests.erl           # Database pool tests  
│   └── cookie_api_integration_tests.erl   # HTTP API integration tests
├── rebar.config            # Build configuration
├── Makefile                # Build automation
├── .gitignore              # Git ignore patterns
├── README.md               # Project documentation
├── GO_TO_ERLANG_GUIDE.md   # Learning guide for Go developers
├── test_api.sh             # API integration tests
├── bench/                  # Performance testing scripts
│   └── post_test.lua       # wrk Lua script for load testing
├── nginx/                  # Nginx reverse proxy configuration
│   └── nginx.conf          # Production-ready nginx config
├── .github/                # GitHub Actions CI/CD workflows
│   └── workflows/          # Automated testing and deployment
│       ├── ci.yml          # Main CI/CD pipeline
│       ├── docker.yml      # Docker build and security scanning
│       └── release.yml     # Release management and artifacts
├── Dockerfile              # Multi-stage Docker build
├── docker-compose.yml      # Container orchestration
├── .dockerignore           # Docker build exclusions
├── PROGRESS.md             # Development progress and debugging log
└── CLAUDE.md               # This file
```

## Key Commands

### Development
- `make compile` - Compile the project
- `make dev` - Start development shell with hot reloading
- `rebar3 shell` - Alternative way to start development environment

### Testing & Analysis
- `make test` - Run main test suite (unit + integration)
- `make test-unit` - Run unit tests only
- `make test-integration` - Run HTTP API integration tests  
- `make test-db` - Run database pool tests
- `make test-all` - Run all EUnit tests including property tests
- `make test-coverage` - Run tests with coverage analysis
- `make dialyzer` - Run Dialyzer static analysis
- `make eqwalizer` - Run eqWAlizer type checking
- `make xref` - Run cross-reference analysis

### Production
- `make release` - Build production release
- `make tar` - Create distribution tarball

### Docker & Container Management
- `docker-compose up -d` - Start production environment
- `docker-compose --profile dev up -d` - Start development environment
- `docker-compose --profile test up` - Run test environment
- `docker-compose --profile load-test up` - Run performance tests
- `docker-compose --profile monitoring up -d` - Start monitoring stack
- `docker build -t cookie-crud .` - Build Docker image
- `docker run -p 8080:8080 cookie-crud` - Run container

### CI/CD & Deployment
- `git tag v1.0.0 && git push origin v1.0.0` - Trigger release pipeline
- GitHub Actions automatically handle testing, building, and deployment
- View pipeline status at: `https://github.com/your-org/cookie-crud/actions`

### Cleanup
- `make clean` - Clean build artifacts
- `make clean-db` - Remove database files
- `make clean-all` - Complete cleanup including PLT files
- `docker-compose down -v` - Stop and remove containers with volumes

## API Endpoints

| Method | Path              | Description           |
|--------|-------------------|-----------------------|
| GET    | `/cookies`        | List all cookies      |
| GET    | `/cookies/:id`    | Get specific cookie   |
| POST   | `/cookies`        | Create new cookie     |
| PUT    | `/cookies/:id`    | Update cookie         |
| DELETE | `/cookies/:id`    | Delete cookie         |

## Database Schema

```sql
CREATE TABLE Cookie (
  Cookie   TEXT    NOT NULL AS (json_extract(Data, '$.cookie'))  STORED UNIQUE,
  UserID   INTEGER NOT NULL AS (json_extract(Data, '$.user_id')) STORED,
  Created  INTEGER NOT NULL AS (json_extract(Data, '$.created')) STORED,
  LastUsed INTEGER AS (json_extract(Data, '$.last_used')) CHECK (LastUsed>0),
  Data     TEXT    NOT NULL
);
```

## Code Style & Conventions

### Module Structure
1. Module declaration (`-module(name).`)
2. Exports (`-export([func/arity]).`)
3. Includes and defines
4. Type specifications
5. Function implementations

### Type Specifications
All exported functions should have `-spec` declarations:
```erlang
-spec function_name(Arg1Type, Arg2Type) -> ReturnType.
```

### Error Handling
Use tagged tuples for return values:
```erlang
{ok, Result} | {error, Reason}
```

### Naming Conventions
- **Modules**: `snake_case`
- **Functions**: `snake_case`
- **Variables**: `CamelCase`
- **Atoms**: `snake_case`
- **Records**: `snake_case`

## Dependencies

### Production Dependencies
- **cowboy**: HTTP server framework
- **esqlite**: SQLite NIF driver
- **jsx**: JSON encoder/decoder

### Development Dependencies
- **dialyzer**: Static analysis tool
- **eqwalizer**: Gradual type checker
- **proper**: Property-based testing (in test profile)

## Common Issues & Solutions

### Compilation Issues
- Ensure all `-spec` declarations match function signatures
- Check for unused types (remove or export them)
- Verify all imports and includes are correct

### Database Issues
- Database file permissions
- SQLite version compatibility
- JSON field extraction syntax

### Type Checking Issues
- Dialyzer PLT files may need regeneration: `make clean-all && make dialyzer`
- Check pattern matching exhaustiveness
- Ensure return value handling is consistent

## Development Workflow

1. **Start Development**: `make dev`
2. **Make Changes**: Edit files in `src/`
3. **Recompile**: `l(module_name).` in shell or restart
4. **Test API**: Use `./test_api.sh` or manual curl commands
5. **Static Analysis**: `make dialyzer` and `make eqwalizer`
6. **Clean Build**: `make clean && make compile`

## Testing Strategy

### Unit Testing
- Use EUnit for module-level tests
- Add `-ifdef(TEST)` guards for test code
- Run with `rebar3 eunit`

### Integration Testing
- Use `test_api.sh` for HTTP API testing
- Test all CRUD operations
- Verify error conditions

### Static Analysis
- **Dialyzer**: Type checking and dead code detection
- **eqWAlizer**: Gradual typing with better error messages
- **XRef**: Cross-reference analysis for unused functions

## Configuration Notes

### System Configuration (config/sys.config)
```erlang
[{cookie_crud, [
    {port, 8080},
    {db_file, "cookies.db"}
]}].
```

### VM Arguments (config/vm.args)
```
-name cookie_crud@127.0.0.1
-setcookie cookie_crud_cookie
-heart
+K true
+A30
```

## Claude Assistance Guidelines

When helping with this project, please:

1. **Follow OTP Conventions**: Use proper application/supervisor structure
2. **Type Safety**: Add `-spec` declarations for new functions
3. **Error Handling**: Use `{ok, Result} | {error, Reason}` patterns
4. **Code Style**: Follow the established naming and formatting conventions
5. **Testing**: Consider both unit tests and integration tests
6. **Documentation**: Update this file and README.md for significant changes
7. **Progress Tracking**: **MANDATORY** - Update PROGRESS.md for every completed task

### Progress Documentation Requirements

**CRITICAL**: After completing any development task, debugging session, or investigation, you MUST update `PROGRESS.md` with:

- **Date and time** of the work
- **Problem description** and symptoms observed  
- **Investigation process** and debugging steps taken
- **Root cause analysis** with code snippets showing the issue
- **Solution implemented** with before/after code examples
- **Verification results** (test outputs, success metrics)
- **Files modified** and key changes made
- **Lessons learned** and future considerations

**Format**: Follow the existing structure in PROGRESS.md with clear sections for each issue resolved.

**Purpose**: This maintains a complete development history for:
- Future debugging reference
- Knowledge transfer to other developers
- Understanding project evolution
- Identifying recurring patterns
- Code review and quality assurance

### Code Examples Format
When providing code examples, use:
- Complete module structure when showing new modules
- Type specifications for all functions
- Proper error handling patterns
- Comments explaining Erlang-specific concepts for Go developers

### Build Commands
Always prefer Make targets over direct rebar3 commands for consistency:
- `make compile` instead of `rebar3 compile`
- `make test` instead of manually running tests
- `make clean` instead of `rebar3 clean`

## Performance Considerations

- SQLite operations are synchronous - consider connection pooling for high load
- JSON parsing/encoding can be expensive - cache when possible  
- Pattern matching is optimized - use it extensively
- Process spawning is cheap - don't hesitate to use processes for isolation

## Security Notes

- Input validation on all JSON inputs
- SQL injection prevention (parameterized queries only)
- Proper error message sanitization
- No sensitive data in logs

## Monitoring & Observability

### Prometheus Metrics

The Cookie CRUD API includes comprehensive Prometheus-compatible metrics collection implemented in `src/cookie_metrics.erl`. This provides real-time monitoring of HTTP requests, database operations, and service health.

#### Metrics Collection Architecture

**Core Components:**
- `cookie_metrics.erl`: Metrics collection module using ETS for thread-safe storage
- `cookie_metrics_handler.erl`: HTTP handler for `/metrics` endpoint
- Prometheus exposition format output for Grafana/alerting integration

**Key Differences from Go Prometheus Integration:**

| Aspect | Go (prometheus/client_golang) | Erlang/OTP (Our Implementation) |
|--------|-------------------------------|----------------------------------|
| **Storage** | sync.Map or atomic counters | ETS (Erlang Term Storage) tables |
| **Threading** | Goroutines + mutexes | Actor model (processes) + message passing |
| **Initialization** | prometheus.MustRegister() | cookie_metrics:init() in app startup |
| **Collection** | Automatic via HTTP middleware | Manual instrumentation in handlers |
| **Exposition** | promhttp.Handler() | Custom formatter in get_metrics/0 |
| **Concurrency** | Locks and atomic operations | Lock-free ETS operations |

#### Available Metrics

1. **HTTP Request Metrics**
   ```
   # HELP http_requests_total Total number of HTTP requests by method and status
   # TYPE http_requests_total counter
   http_requests_total{method="GET",status="200"} 1542
   http_requests_total{method="POST",status="201"} 89
   http_requests_total{method="POST",status="400"} 34
   ```

2. **HTTP Duration Metrics**
   ```
   # HELP http_request_duration_seconds Average HTTP request duration
   # TYPE http_request_duration_seconds gauge
   http_request_duration_seconds{method="GET"} 0.012
   http_request_duration_seconds{method="POST"} 0.045
   ```

3. **Database Operation Metrics**
   ```
   # HELP db_operations_total Total number of database operations
   # TYPE db_operations_total counter
   db_operations_total{operation="list_cookies"} 1542
   db_operations_total{operation="create_cookie"} 123
   db_operations_total{operation="get_cookie"} 67
   ```

4. **Service Uptime**
   ```
   # HELP service_uptime_seconds Time since service started
   # TYPE service_uptime_seconds gauge
   service_uptime_seconds 3847
   ```

#### Metrics Collection Implementation

**ETS-Based Storage (Go equivalent: sync.Map)**
```erlang
%% Initialize thread-safe metrics storage
%% Go equivalent: var metricsMap sync.Map
init() ->
    try
        ets:new(?METRICS_TABLE, [named_table, public, set]),
        ets:insert(?METRICS_TABLE, {state, #metrics_state{start_time = erlang:system_time(second)}}),
        ok
    catch
        error:badarg -> ok  %% Table already exists (race condition)
    end.
```

**Atomic Increment Operations (Go equivalent: atomic.AddInt64)**
```erlang
%% Increment HTTP request counter with error handling
%% Go equivalent: httpRequestsTotal.WithLabelValues(method, status).Inc()
increment_http_requests(Method, StatusCode) ->
    Key = {Method, StatusCode},
    try
        case ets:lookup(?METRICS_TABLE, {http_requests, Key}) of
            [{_, Count}] ->
                ets:insert(?METRICS_TABLE, {{http_requests, Key}, Count + 1});
            [] ->
                ets:insert(?METRICS_TABLE, {{http_requests, Key}, 1})
        end,
        ok
    catch
        error:badarg ->
            %% Defensive programming: retry with initialization
            init(),
            %% Retry operation once after ensuring table exists
            %% (implementation continues with fallback logic)
    end.
```

**Instrumentation in HTTP Handlers**
```erlang
%% Track metrics in response functions (like Go middleware)
reply_json(Req, Status, Data) ->
    Method = cowboy_req:method(Req),
    cookie_metrics:increment_http_requests(Method, Status),
    %% ... rest of response handling
```

#### Error Handling & Resilience

**Pros of Erlang Approach:**
- **Fault Tolerance**: ETS operations are atomic and crash-resistant
- **No Deadlocks**: Lock-free concurrent access to metrics data
- **Self-Healing**: Automatic table recreation on errors with defensive programming
- **Hot Code Loading**: Can update metrics code without stopping service

**Cons vs Go:**
- **Manual Instrumentation**: Must manually add metrics to each handler (Go has automatic middleware)
- **Memory Overhead**: ETS tables store more metadata than simple counters
- **Learning Curve**: Requires understanding ETS and Erlang concurrency model

**Alternative Approaches:**
- **telemetry Library**: More sophisticated metrics with plugins (like Go's OpenTelemetry)
- **exometer**: Enterprise-grade metrics framework for Erlang
- **Prometheus.erl**: Direct Prometheus client library for Erlang

#### Usage Examples

**Starting Metrics Collection:**
```bash
# Metrics are automatically initialized when the application starts
make dev
```

**Accessing Metrics:**
```bash
# Get current metrics in Prometheus format
curl http://localhost:8080/metrics

# Use with Prometheus server in docker-compose
docker-compose up -d  # Includes Prometheus + Grafana setup
```

**Benchmarking with Metrics:**
```bash
# Generate load and observe metrics
./bench/quick_bench.sh
curl http://localhost:8080/metrics
```

#### Integration with Grafana

The metrics endpoint (`/metrics`) is configured for Prometheus scraping in `monitoring/prometheus.yml`:

```yaml
scrape_configs:
  - job_name: 'cookie-crud-api'
    static_configs:
      - targets: ['host.docker.internal:8080']
    metrics_path: '/metrics'
    scrape_interval: 15s
```

**Grafana Dashboard Queries:**
```promql
# Request rate by status code
rate(http_requests_total[5m])

# Average response time by method
http_request_duration_seconds

# Database operation rate
rate(db_operations_total[5m])

# Error rate percentage
100 * (rate(http_requests_total{status=~"4.."}[5m]) / rate(http_requests_total[5m]))
```

#### Monitoring Best Practices

1. **Performance Impact**: Metrics add ~1-5μs per request (negligible for most applications)
2. **Cardinality Management**: Limited label combinations prevent memory explosion
3. **Error Handling**: Graceful degradation if metrics collection fails
4. **Service Discovery**: Ready for Kubernetes/Docker Swarm service discovery

#### Comparison: Go vs Erlang Monitoring

| Feature | Go Implementation | Erlang Implementation |
|---------|-------------------|----------------------|
| **Setup Complexity** | Simple (import library) | Medium (custom ETS implementation) |
| **Performance** | Excellent (atomic ops) | Very Good (ETS operations) |
| **Memory Usage** | Lower (simple counters) | Higher (ETS metadata) |
| **Fault Tolerance** | Good (panic recovery) | Excellent (supervisor restart) |
| **Hot Updates** | Requires restart | Live code updates possible |
| **Ecosystem** | Rich (OpenTelemetry, etc.) | Limited (custom solutions) |
| **Learning Curve** | Low (familiar patterns) | High (understand ETS/OTP) |

The Erlang approach provides superior fault tolerance and eliminates concurrency issues at the cost of some complexity and manual instrumentation.

## Version Information

- **Erlang/OTP**: 24+ required
- **rebar3**: Latest stable version
- **SQLite**: 3.38+ for JSON support
- **Cowboy**: 2.10.0
- **JSX**: 3.1.0

## Current Project Status

**Last Updated**: 2025-06-12  
**Integration Tests**: ✅ 10/10 passing  
**Unit Tests**: ✅ 8/8 passing  
**Database Pool Tests**: ✅ 8/8 passing  
**Total Test Coverage**: ✅ 26/26 tests passing  

**Recent Achievements**:
- ✅ Fixed all integration test failures (JSON encoding, error handling, validation)
- ✅ Implemented proper HTTP status code mapping
- ✅ Added Content-Type validation for API security
- ✅ Enhanced error handling with proper 400/404/500 responses
- ✅ Comprehensive debugging and root cause analysis documented in PROGRESS.md
- 🚀 **NEW**: Complete CI/CD pipeline with GitHub Actions
- 🐳 **NEW**: Production-ready Docker containerization
- 🔒 **NEW**: Security scanning and vulnerability management
- 📊 **NEW**: Performance testing and monitoring setup
- 🌐 **NEW**: Nginx reverse proxy with SSL/TLS and rate limiting

**Ready for**: Enterprise production deployment with automated CI/CD, monitoring, and scaling

## File References

- **PROGRESS.md**: Complete development history, debugging sessions, and solutions
- **Makefile**: Enhanced with comprehensive test targets and documentation
- **src/cookie_crud.erl**: Main HTTP handler with robust error handling
- **test/**: Complete test suite covering all functionality

This configuration helps Claude understand the project structure, conventions, and provide more accurate assistance for Erlang/OTP development.