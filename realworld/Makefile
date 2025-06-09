.PHONY: compile shell test dialyzer format release run db-create db-migrate db-seed clean help

# Default target
all: compile

# Help target
help:
	@echo "Available targets:"
	@echo "  compile    - Compile the project"
	@echo "  shell      - Start an Erlang shell with the app loaded"
	@echo "  test       - Run all tests"
	@echo "  dialyzer   - Run dialyzer for type checking"
	@echo "  format     - Format code using erlfmt"
	@echo "  release    - Build a release"
	@echo "  run        - Run the application"
	@echo "  db-create  - Create database"
	@echo "  db-migrate - Run database migrations"
	@echo "  db-seed    - Seed database with test data"
	@echo "  clean      - Clean build artifacts"

# Compile the project
compile:
	rebar3 compile

# Start an Erlang shell with the app loaded
shell:
	rebar3 shell

# Run all tests
test:
	rebar3 eunit
	rebar3 ct

# Run dialyzer for type checking
dialyzer:
	rebar3 dialyzer

# Format code using erlfmt
format:
	rebar3 fmt

# Build a release
release:
	rebar3 as prod release

# Run the application
run:
	rebar3 shell --apps realworld

# Database operations
db-create:
	@echo "Creating PostgreSQL database 'realworld_dev'..."
	createdb -h localhost -p 5433 -U myuser realworld_dev || echo "Database might already exist"

db-migrate:
	@echo "Running database migrations..."
	psql -h localhost -p 5433 -U myuser -d realworld_dev -f priv/sql/001_create_users.sql
	psql -h localhost -p 5433 -U myuser -d realworld_dev -f priv/sql/002_create_articles.sql

db-seed:
	@echo "Seeding database with test data..."
	psql -h localhost -p 5433 -U myuser -d realworld_dev -f priv/sql/seed.sql

# Clean build artifacts
clean:
	rebar3 clean

# Development helper targets
dev-setup: db-create db-migrate compile
	@echo "Development environment setup complete!"

dev-reset: clean db-drop db-create db-migrate
	@echo "Development environment reset complete!"

db-drop:
	@echo "Dropping PostgreSQL database 'realworld_dev'..."
	dropdb -h localhost -p 5433 -U myuser realworld_dev || echo "Database might not exist"

# Quick development cycle
dev: format compile test
	@echo "Development cycle complete!" 