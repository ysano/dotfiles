---
description: "Setup application performance monitoring"
---

## Instructions

1. **Performance Monitoring Strategy**
   - Define key performance indicators (KPIs) and service level objectives (SLOs)
   - Identify critical user journeys and performance bottlenecks
   - Plan monitoring architecture and data collection strategy
   - Assess existing monitoring infrastructure and integration points
   - Define alerting thresholds and escalation procedures

2. **Application Performance Monitoring (APM)**
   - Set up comprehensive APM monitoring:

   **Node.js APM with New Relic:**
   ```javascript
   // newrelic.js
   exports.config = {
     app_name: [process.env.NEW_RELIC_APP_NAME || 'My Application'],
     license_key: process.env.NEW_RELIC_LICENSE_KEY,
     distributed_tracing: {
       enabled: true
     },
     transaction_tracer: {
   # ... (55 lines total, truncated)
   ```

   **Datadog APM Integration:**
   ```javascript
   // datadog-tracer.js
   const tracer = require('dd-trace').init({
     service: 'my-application',
     env: process.env.NODE_ENV,
     version: process.env.APP_VERSION,
     logInjection: true,
     runtimeMetrics: true,
     profiling: true,
   # ... (62 lines total, truncated)
   ```

3. **Real User Monitoring (RUM)**
   - Implement client-side performance tracking:

   **Web Vitals Monitoring:**
   ```javascript
   // performance-monitor.js
   import { getCLS, getFID, getFCP, getLCP, getTTFB } from 'web-vitals';

   class RealUserMonitoring {
     constructor() {
       this.metrics = {};
       this.setupWebVitals();
       this.setupCustomMetrics();
   # ... (98 lines total, truncated)
   ```

   **React Performance Monitoring:**
   ```javascript
   // react-performance.js
   import { Profiler } from 'react';

   class ReactPerformanceMonitor {
     static ProfilerWrapper = ({ id, children }) => {
       const onRenderCallback = (id, phase, actualDuration, baseDuration, startTime, commitTime) => {
         // Track component render performance
         if (actualDuration > 100) { // Renders taking >100ms
   # ... (61 lines total, truncated)
   ```

4. **Server Performance Monitoring**
   - Monitor server-side performance metrics:

   **System Metrics Collection:**
   ```javascript
   // system-monitor.js
   const os = require('os');
   const process = require('process');
   const v8 = require('v8');

   class SystemMonitor {
     constructor() {
       this.startTime = Date.now();
   # ... (78 lines total, truncated)
   ```

   **Express.js Performance Middleware:**
   ```javascript
   // performance-middleware.js
   const responseTime = require('response-time');
   const promClient = require('prom-client');

   // Prometheus metrics
   const httpRequestDuration = new promClient.Histogram({
     name: 'http_request_duration_seconds',
     help: 'Duration of HTTP requests in seconds',
   # ... (63 lines total, truncated)
   ```

5. **Database Performance Monitoring**
   - Monitor database query performance:

   **Query Performance Tracking:**
   ```javascript
   // db-performance.js
   const { Pool } = require('pg');

   class DatabasePerformanceMonitor {
     constructor(pool) {
       this.pool = pool;
       this.slowQueryThreshold = 1000; // 1 second
       this.queryStats = new Map();
   # ... (123 lines total, truncated)
   ```

6. **Error Tracking and Monitoring**
   - Implement comprehensive error monitoring:

   **Error Tracking Setup:**
   ```javascript
   // error-monitor.js
   const Sentry = require('@sentry/node');
   const Integrations = require('@sentry/integrations');

   class ErrorMonitor {
     static initialize() {
       Sentry.init({
         dsn: process.env.SENTRY_DSN,
   # ... (96 lines total, truncated)
   ```

7. **Custom Metrics and Dashboards**
   - Create custom performance dashboards:

   **Prometheus Metrics:**
   ```javascript
   // prometheus-metrics.js
   const promClient = require('prom-client');

   class CustomMetrics {
     constructor() {
       // Register default metrics
       promClient.register.setDefaultLabels({
         app: process.env.APP_NAME || 'my-app',
   # ... (88 lines total, truncated)
   ```

8. **Alerting and Notification System**
   - Set up intelligent alerting:

   **Alert Manager:**
   ```javascript
   // alert-manager.js
   const nodemailer = require('nodemailer');
   const slack = require('@slack/webhook');

   class AlertManager {
     constructor() {
       this.emailTransporter = nodemailer.createTransporter({
         // Email configuration
   # ... (139 lines total, truncated)
   ```

9. **Performance Testing Integration**
   - Integrate with performance testing:

   **Load Test Monitoring:**
   ```javascript
   // load-test-monitor.js
   class LoadTestMonitor {
     constructor() {
       this.testResults = [];
       this.baselineMetrics = null;
     }

     async runPerformanceTest(testConfig) {
   # ... (92 lines total, truncated)
   ```

10. **Performance Optimization Recommendations**
    - Generate actionable performance insights:

    **Performance Analyzer:**
    ```javascript
    // performance-analyzer.js
    class PerformanceAnalyzer {
      constructor() {
        this.metrics = [];
        this.thresholds = {
          responseTime: { good: 200, warning: 1000, critical: 3000 },
          memoryUsage: { good: 0.6, warning: 0.8, critical: 0.9 },
          cpuUsage: { good: 0.5, warning: 0.7, critical: 0.85 },
   # ... (125 lines total, truncated)
    ```

See also: `/performance:performance-audit` for identifying bottlenecks before setting up monitoring.
