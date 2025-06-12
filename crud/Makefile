.PHONY: all compile clean deps shell test test-unit test-integration test-db test-all test-coverage dialyzer eqwalizer xref start stop dev clean-db clean-all bench quick-bench help

# Default target
all: compile

# Compile the project
compile:
	@rebar3 compile

# Clean build artifacts
clean:
	@rebar3 clean

# Fetch dependencies
deps:
	@rebar3 get-deps

# Start Erlang shell with project loaded
shell:
	@rebar3 shell

# Run comprehensive test suite
test: test-unit test-integration
	@echo "All tests completed successfully!"

# Run unit tests only
test-unit: compile
	@echo "Running unit tests..."
	@rebar3 eunit --module=cookie_crud_tests
	@echo "Unit tests completed!"

# Run integration tests only
test-integration: compile
	@echo "Running integration tests..."
	@rebar3 eunit --module=cookie_api_integration_tests
	@echo "Integration tests completed!"

# Run database pool tests only
test-db: compile
	@echo "Running database pool tests..."
	@rebar3 eunit --module=cookie_db_pool_tests
	@echo "Database pool tests completed!"

# Run all EUnit tests
test-all: compile
	@echo "Running all EUnit tests..."
	@rebar3 eunit
	@echo "All EUnit tests completed!"

# Run tests with coverage analysis
test-coverage: compile
	@echo "Running tests with coverage analysis..."
	@rebar3 cover -v
	@echo "Coverage analysis completed!"

# Run API integration tests using shell script
test-api: compile
	@echo "Running API shell script tests..."
	@./test_api.sh
	@echo "API tests completed!"

# Run Dialyzer for static analysis
dialyzer:
	@rebar3 dialyzer

# Run eqWAlizer for type checking
eqwalizer:
	@echo "eqWAlizer is currently disabled due to installation issues"
	@echo "Enable it in rebar.config when your environment supports it"
	@echo "For now, use 'make dialyzer' for static analysis"

# Run cross reference analysis
xref:
	@rebar3 xref

# Start the application
start: compile
	@echo "Starting Cookie CRUD API application..."
	@rebar3 shell --start-clean --apps cookie_crud --eval "application:ensure_all_started(cookie_crud)"

# Start in development mode
dev: compile
	@echo "Starting development environment..."
	@rebar3 shell

# Stop the server (for compatibility)
stop:
	@echo "Use Ctrl+C to stop the server when running in shell mode"

# Clean database
clean-db:
	@echo "Cleaning database..."
	@rm -f cookies.db

# Complete clean (including database and PLT files)
clean-all: clean clean-db
	@echo "Cleaning PLT files..."
	@rm -rf _build/default/rebar3_*_plt
	@echo "Complete cleanup done"

# Run comprehensive benchmarks
bench: compile
	@echo "Running comprehensive benchmarks..."
	@./bench/run_benchmarks.sh

# Run quick benchmark test
quick-bench: compile
	@echo "Running quick benchmark test..."
	@./bench/quick_bench.sh

# Build release
release:
	@rebar3 as prod release

# Create tarball
tar:
	@rebar3 as prod tar

# Help target
help:
	@echo "Available targets:"
	@echo ""
	@echo "Build and Development:"
	@echo "  all               - Build the project (default)"
	@echo "  compile           - Compile source code"
	@echo "  deps              - Fetch dependencies"
	@echo "  shell             - Start Erlang shell with project"
	@echo "  start             - Start application"
	@echo "  dev               - Start development shell"
	@echo "  stop              - Stop server info"
	@echo ""
	@echo "Testing:"
	@echo "  test              - Run main test suite (unit + integration)"
	@echo "  test-unit         - Run unit tests only"
	@echo "  test-integration  - Run HTTP API integration tests"
	@echo "  test-db           - Run database pool tests"
	@echo "  test-all          - Run all EUnit tests including property tests"
	@echo "  test-coverage     - Run tests with coverage analysis"
	@echo "  test-api          - Run shell script API tests"
	@echo ""
	@echo "Analysis:"
	@echo "  dialyzer          - Run Dialyzer static analysis"
	@echo "  eqwalizer         - Run eqWAlizer type checking"
	@echo "  xref              - Run cross reference analysis"
	@echo ""
	@echo "Performance:"
	@echo "  bench             - Run comprehensive benchmarks with wrk"
	@echo "  quick-bench       - Run quick benchmark test"
	@echo ""
	@echo "Cleanup:"
	@echo "  clean             - Clean build files"
	@echo "  clean-db          - Remove database file"
	@echo "  clean-all         - Clean everything including PLT files"
	@echo ""
	@echo "Production:"
	@echo "  release           - Build production release"
	@echo "  tar               - Create release tarball"
	@echo ""
	@echo "  help              - Show this help"