# RealWorld Erlang

> ### Erlang/OTP implementation of the RealWorld spec and API

This codebase was created to demonstrate a fully fledged backend application built with **Erlang/OTP** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the **Erlang/OTP** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

## How it works

This application is built using:

- **Erlang/OTP** - Core language and runtime
- **Cowboy** - HTTP server and routing
- **PostgreSQL** - Database with epgsql driver
- **JWT** - Authentication using jwerl
- **BCrypt** - Password hashing
- **JSX** - JSON encoding/decoding
- **Poolboy** - Database connection pooling
- **Rebar3** - Build tool and dependency management

### Architecture

The application follows OTP principles with a clear separation of concerns:

- **apps/realworld/src/** - Main application code
  - **handlers/** - HTTP request handlers (Cowboy handlers)
  - **models/** - Data models and database operations
  - **db/** - Database connection and query utilities
  - **utils/** - Utility modules (JWT, auth, etc.)
- **config/** - Application configuration
- **apps/realworld/priv/sql/** - Database migrations and seeds

## Getting started

### Prerequisites

- Erlang/OTP 24+ 
- PostgreSQL 12+
- Rebar3
- Make

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourname/realworld-erlang.git
cd realworld-erlang
```

2. Install dependencies:
```bash
rebar3 deps
```

3. Set up the database:
```bash
make db-create
make db-migrate
```

4. Compile and run:
```bash
make compile
make run
```

The API server will be running at `http://localhost:8080`.

### Available Make Targets

- `make compile` - Compile the project
- `make shell` - Start an Erlang shell with the app loaded
- `make test` - Run all tests
- `make dialyzer` - Run dialyzer for type checking
- `make format` - Format code using erlfmt
- `make release` - Build a release
- `make run` - Run the application
- `make db-create` - Create database
- `make db-migrate` - Run database migrations
- `make db-seed` - Seed database with test data
- `make clean` - Clean build artifacts
- `make dev-setup` - Complete development environment setup
- `make help` - Show all available targets

## Configuration

Configure the application by editing `config/sys.config`:

```erlang
[
    {realworld, [
        {http_port, 8080},
        {database, [
            {host, "localhost"},
            {port, 5432},
            {database, "realworld_dev"},
            {username, "postgres"},
            {password, ""},
            {pool_size, 10}
        ]},
        {jwt, [
            {secret, "your-secret-key-change-in-production"},
            {expiry_hours, 24}
        ]}
    ]}
].
```

## API Endpoints

### Authentication

- `POST /api/users` - User registration
- `POST /api/users/login` - User login

### User Management

- `GET /api/user` - Get current user (requires auth)
- `PUT /api/user` - Update current user (requires auth)

### Profiles

- `GET /api/profiles/:username` - Get user profile
- `POST /api/profiles/:username/follow` - Follow user (requires auth)
- `DELETE /api/profiles/:username/follow` - Unfollow user (requires auth)

### Articles

- `GET /api/articles` - List articles
- `GET /api/articles/feed` - Get user feed (requires auth)
- `GET /api/articles/:slug` - Get article
- `POST /api/articles` - Create article (requires auth)
- `PUT /api/articles/:slug` - Update article (requires auth)
- `DELETE /api/articles/:slug` - Delete article (requires auth)

### Article Interactions

- `POST /api/articles/:slug/favorite` - Favorite article (requires auth)
- `DELETE /api/articles/:slug/favorite` - Unfavorite article (requires auth)

### Comments

- `GET /api/articles/:slug/comments` - Get article comments
- `POST /api/articles/:slug/comments` - Add comment (requires auth)
- `DELETE /api/articles/:slug/comments/:id` - Delete comment (requires auth)

### Tags

- `GET /api/tags` - Get all tags

## Testing

### Unit Tests

Run unit tests with:
```bash
make test
```

### Integration Testing

Test against the RealWorld Postman collection:
```bash
# Import the collection from api/Conduit.postman_collection.json
# Set the base URL to http://localhost:8080
```

### Manual Testing

Example user registration:
```bash
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{
    "user": {
      "username": "jake",
      "email": "jake@jake.jake",
      "password": "jakejake"
    }
  }'
```

Example user login:
```bash
curl -X POST http://localhost:8080/api/users/login \
  -H "Content-Type: application/json" \
  -d '{
    "user": {
      "email": "jake@jake.jake",
      "password": "jakejake"
    }
  }'
```

## Development

### Code Style

The project uses `erlfmt` for code formatting:
```bash
make format
```

### Type Checking

Run Dialyzer for static analysis:
```bash
make dialyzer
```

### Development Workflow

1. Make changes to the code
2. Format code: `make format`
3. Compile: `make compile`
4. Run tests: `make test`
5. Type check: `make dialyzer`

## Production Deployment

1. Build a production release:
```bash
make release
```

2. The release will be available in `_build/prod/rel/realworld/`

3. Configure production settings in `config/sys.config`

4. Set up PostgreSQL database

5. Run migrations:
```bash
make db-migrate
```

6. Start the release:
```bash
./_build/prod/rel/realworld/bin/realworld start
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Run the test suite
6. Submit a pull request

## License

This project is licensed under the Apache 2.0 License. 