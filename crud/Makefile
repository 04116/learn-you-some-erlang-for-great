.PHONY: all compile clean deps shell test dialyzer eqwalizer xref start stop dev clean-db clean-all bench quick-bench help

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

# Run tests
test: compile
	@echo "Running API tests..."
	@./test_api.sh

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
	@echo "  all       - Build the project"
	@echo "  compile   - Compile source code"
	@echo "  deps      - Fetch dependencies"
	@echo "  shell     - Start Erlang shell with project"
	@echo "  start     - Start application"
	@echo "  dev       - Start development shell"
	@echo "  stop      - Stop server info"
	@echo "  test      - Run API tests"
	@echo "  dialyzer  - Run Dialyzer static analysis"
	@echo "  eqwalizer - Run eqWAlizer type checking"
	@echo "  xref      - Run cross reference analysis"
	@echo "  clean     - Clean build files"
	@echo "  clean-db  - Remove database file"
	@echo "  clean-all - Clean everything including PLT files"
	@echo "  bench     - Run comprehensive benchmarks with wrk"
	@echo "  quick-bench - Run quick benchmark test"
	@echo "  release   - Build production release"
	@echo "  tar       - Create release tarball"
	@echo "  help      - Show this help"