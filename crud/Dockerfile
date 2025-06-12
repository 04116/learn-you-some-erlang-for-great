# Multi-stage Docker build for Cookie CRUD API
# Production-ready Erlang/OTP application container

# =============================================================================
# Stage 1: Build Environment
# =============================================================================
FROM erlang:26-alpine AS builder

# Set working directory
WORKDIR /app

# Install build dependencies
RUN apk add --no-cache \
    git \
    sqlite \
    sqlite-dev \
    build-base \
    openssl-dev \
    ncurses-dev

# Copy rebar configuration files
COPY rebar.config rebar.lock ./

# Download dependencies
RUN rebar3 get-deps

# Copy source code
COPY src/ src/
COPY config/ config/
COPY Makefile ./

# Copy application resource file
COPY src/cookie_crud.app.src src/

# Compile the application
RUN make compile

# Build production release
RUN make release

# Create distribution tarball
RUN make tar

# =============================================================================
# Stage 2: Runtime Environment
# =============================================================================
FROM erlang:26-alpine AS runtime

# Install runtime dependencies
RUN apk add --no-cache \
    ncurses \
    openssl \
    sqlite \
    bash \
    curl \
    libstdc++ \
    libgcc \
    && addgroup -g 1000 cookie_crud \
    && adduser -D -u 1000 -G cookie_crud cookie_crud

# Set working directory
WORKDIR /opt/cookie_crud

# Copy release from builder stage
COPY --from=builder /app/_build/prod/rel/cookie_crud/ ./

# Create data directory and set permissions
RUN mkdir -p /opt/cookie_crud/data \
    && chown -R cookie_crud:cookie_crud /opt/cookie_crud

# Switch to non-root user
USER cookie_crud

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/cookies || exit 1

# Set environment variables
ENV COOKIE_CRUD_PORT=8080
ENV COOKIE_CRUD_DB_FILE="/opt/cookie_crud/data/cookies.db"
ENV COOKIE_CRUD_DB_POOL_SIZE=10

# Default command
CMD ["./bin/cookie_crud", "foreground"]

# =============================================================================
# Stage 3: Development Environment (optional)
# =============================================================================
FROM builder AS development

# Install additional development tools
RUN apk add --no-cache \
    inotify-tools \
    vim \
    less

# Install test dependencies
RUN rebar3 as test get-deps

# Copy test files
COPY test/ test/

# Set development environment variables
ENV COOKIE_CRUD_ENV=development
ENV COOKIE_CRUD_PORT=8080
ENV COOKIE_CRUD_DB_FILE="cookies_dev.db"

# Expose debug ports
EXPOSE 8080 4369 9001-9005

# Development command with hot reloading
CMD ["make", "dev"]

# =============================================================================
# Metadata
# =============================================================================
LABEL org.opencontainers.image.title="Cookie CRUD API"
LABEL org.opencontainers.image.description="RESTful API for cookie management built with Erlang/OTP and Cowboy"
LABEL org.opencontainers.image.version="1.0.0"
LABEL org.opencontainers.image.authors="Cookie CRUD Team"
LABEL org.opencontainers.image.source="https://github.com/your-org/cookie-crud"
LABEL org.opencontainers.image.documentation="https://github.com/your-org/cookie-crud/blob/main/README.md"
LABEL org.opencontainers.image.vendor="Your Organization"
LABEL org.opencontainers.image.licenses="MIT"