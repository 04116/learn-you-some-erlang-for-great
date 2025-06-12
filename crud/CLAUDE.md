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

### Cleanup
- `make clean` - Clean build artifacts
- `make clean-db` - Remove database files
- `make clean-all` - Complete cleanup including PLT files

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
- Fixed all integration test failures (JSON encoding, error handling, validation)
- Implemented proper HTTP status code mapping
- Added Content-Type validation for API security
- Enhanced error handling with proper 400/404/500 responses
- Comprehensive debugging and root cause analysis documented in PROGRESS.md

**Ready for**: Production deployment, additional feature development, performance optimization

## File References

- **PROGRESS.md**: Complete development history, debugging sessions, and solutions
- **Makefile**: Enhanced with comprehensive test targets and documentation
- **src/cookie_crud.erl**: Main HTTP handler with robust error handling
- **test/**: Complete test suite covering all functionality

This configuration helps Claude understand the project structure, conventions, and provide more accurate assistance for Erlang/OTP development.