# Monitoring Setup for Cookie CRUD API

This directory contains the complete monitoring stack configuration for the Cookie CRUD API, including Prometheus metrics collection, Grafana dashboards, and alerting rules.

## Components

### Prometheus Configuration
- **`prometheus.yml`**: Main Prometheus configuration with scrape jobs
- **`alert_rules.yml`**: Alerting rules for application and infrastructure monitoring

### Grafana Configuration
- **`grafana/datasources/prometheus.yml`**: Automatic Prometheus data source provisioning
- **`grafana/dashboards/dashboard.yml`**: Dashboard provisioning configuration
- **`grafana/dashboards/cookie-crud-api.json`**: Main application dashboard
- **`grafana/dashboards/infrastructure.json`**: Infrastructure and system metrics dashboard

## Quick Start

### 1. Start the Monitoring Stack

```bash
# Start the complete monitoring stack
docker-compose --profile monitoring up -d

# Check all services are running
docker-compose --profile monitoring ps
```

### 2. Access the Dashboards

- **Grafana**: http://localhost:3000
  - Username: `admin`
  - Password: `admin`
  - Pre-configured dashboards will be automatically loaded

- **Prometheus**: http://localhost:9090
  - Query interface for metrics exploration
  - Alerting rules status at http://localhost:9090/alerts

- **Node Exporter**: http://localhost:9100/metrics
  - System metrics endpoint

- **cAdvisor**: http://localhost:8081
  - Container metrics and performance data

### 3. Available Dashboards

#### Cookie CRUD API Dashboard
- **Request Rate**: HTTP requests per second by method and status
- **Service Status**: Real-time health check status
- **Response Time**: 95th and 50th percentile latency
- **Error Rate**: 4xx and 5xx error percentages
- **Memory Usage**: Container memory consumption
- **CPU Usage**: Container CPU utilization
- **Database Connection Pool**: Active and idle database connections
- **Database Query Time**: Query performance metrics

#### Infrastructure Dashboard
- **Service Health Status**: Overall system health overview
- **Container CPU Usage**: CPU usage across all containers
- **Container Memory Usage**: Memory consumption by container
- **Network I/O**: Inbound and outbound network traffic
- **Disk Usage**: Filesystem utilization
- **Service Uptime**: Historical uptime tracking

## Metrics Collected

### Application Metrics (Port 8080/metrics)
```
# HTTP request metrics
http_requests_total{method, status, endpoint}
http_request_duration_seconds{method, endpoint}

# Database metrics
db_connection_pool_active
db_connection_pool_idle
db_connection_pool_max
db_query_duration_seconds{operation}

# Custom business metrics
cookies_created_total
cookies_updated_total
cookies_deleted_total
```

### System Metrics
- **Node Exporter**: CPU, memory, disk, network statistics
- **cAdvisor**: Container resource usage and performance
- **Nginx**: Request rates, response times, error rates

## Alerting Rules

### Critical Alerts
- **CookieCrudAPIDown**: API service is unreachable
- **HighErrorRate**: 5xx error rate > 5%
- **DatabaseConnectionPoolExhausted**: All DB connections in use

### Warning Alerts
- **HighResponseTime**: 95th percentile > 1 second
- **HighMemoryUsage**: Memory usage > 80%
- **HighCPUUsage**: CPU usage > 80%
- **SlowDatabaseQueries**: Database query time > 0.5 seconds

### Infrastructure Alerts
- **NginxDown**: Reverse proxy is down
- **DiskSpaceUsageHigh**: Disk usage > 80%
- **DiskSpaceUsageCritical**: Disk usage > 90%

## Configuration Customization

### Adding New Metrics
1. Update `prometheus.yml` to add new scrape targets
2. Restart Prometheus: `docker-compose restart prometheus`

### Creating Custom Dashboards
1. Create dashboard in Grafana UI
2. Export JSON and save to `grafana/dashboards/`
3. Restart Grafana to auto-load: `docker-compose restart grafana`

### Modifying Alert Rules
1. Edit `alert_rules.yml`
2. Reload Prometheus configuration:
   ```bash
   curl -X POST http://localhost:9090/-/reload
   ```

### Production Configuration

#### External Prometheus/Grafana
For production, you may want to use external monitoring services:

```yaml
# In prometheus.yml - add remote_write for external storage
remote_write:
  - url: "https://prometheus-remote-write-endpoint.example.com/api/v1/write"
    basic_auth:
      username: "your-username"
      password: "your-password"
```

#### Alertmanager Integration
To receive alerts via email/Slack/PagerDuty:

1. Add Alertmanager service to docker-compose.yml
2. Configure alert routing in alertmanager.yml
3. Update prometheus.yml alerting section

## Troubleshooting

### Common Issues

#### Prometheus Targets Down
```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets

# Check service connectivity
docker-compose exec prometheus ping cookie-crud
```

#### Grafana Dashboard Not Loading
```bash
# Check Grafana logs
docker-compose logs grafana

# Verify dashboard files
ls -la monitoring/grafana/dashboards/
```

#### Missing Container Metrics
```bash
# Ensure cAdvisor has proper permissions
docker-compose logs cadvisor

# Check if Docker socket is accessible
docker-compose exec cadvisor ls -la /var/run/docker.sock
```

### Performance Tuning

#### Prometheus Storage
```yaml
# In prometheus.yml - adjust retention
storage:
  tsdb:
    retention.time: 30d  # Increase for longer history
    retention.size: 100GB  # Adjust based on disk space
```

#### Grafana Performance
```bash
# Add to grafana environment variables
- GF_DATABASE_TYPE=mysql  # Use external database for better performance
- GF_SECURITY_COOKIE_SAMESITE=lax
```

## Security Considerations

### Access Control
- Change default Grafana password in production
- Use HTTPS for external access
- Implement network-level access restrictions

### Data Privacy
- Ensure no sensitive data in metric labels
- Configure proper retention policies
- Use secure communication between components

## Monitoring Best Practices

1. **Set up proper alerting** - Don't just collect metrics, act on them
2. **Monitor the golden signals** - Latency, traffic, errors, saturation
3. **Use dashboards wisely** - Focus on actionable metrics
4. **Regular review** - Periodically review and update alert thresholds
5. **Document runbooks** - Create procedures for each alert

## Integration with CI/CD

The monitoring configuration is automatically tested in the CI/CD pipeline:

```bash
# Test monitoring stack in CI
docker-compose --profile monitoring up -d
docker-compose --profile monitoring exec prometheus promtool check config /etc/prometheus/prometheus.yml
```

For more information, see the main project documentation in `/PROGRESS.md`.