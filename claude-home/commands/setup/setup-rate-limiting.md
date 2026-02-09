---
description: "Implement API rate limiting"
---

## Instructions

1. **Rate Limiting Strategy and Planning**
   - Analyze API endpoints and traffic patterns
   - Define rate limiting policies for different user types and endpoints
   - Plan for distributed rate limiting across multiple servers
   - Consider different rate limiting algorithms (token bucket, sliding window, etc.)
   - Design rate limiting bypass mechanisms for trusted clients

2. **Express.js Rate Limiting Implementation**
   - Set up comprehensive rate limiting middleware:

   **Basic Rate Limiting Setup:**
   ```javascript
   // middleware/rate-limiter.js
   const rateLimit = require('express-rate-limit');
   const RedisStore = require('rate-limit-redis');
   const Redis = require('ioredis');

   class RateLimiter {
     constructor() {
       this.redis = new Redis(process.env.REDIS_URL);
   # ... (121 lines total, truncated)
   ```

3. **Advanced Rate Limiting Algorithms**
   - Implement sophisticated rate limiting strategies:

   **Token Bucket Implementation:**
   ```javascript
   // rate-limiters/token-bucket.js
   class TokenBucket {
     constructor(capacity, refillRate, refillPeriod = 1000) {
       this.capacity = capacity;
       this.tokens = capacity;
       this.refillRate = refillRate;
       this.refillPeriod = refillPeriod;
       this.lastRefill = Date.now();
   # ... (104 lines total, truncated)
   ```

   **Sliding Window Rate Limiter:**
   ```javascript
   // rate-limiters/sliding-window.js
   class SlidingWindowRateLimiter {
     constructor(redis, windowSize, maxRequests) {
       this.redis = redis;
       this.windowSize = windowSize; // in milliseconds
       this.maxRequests = maxRequests;
     }

   # ... (63 lines total, truncated)
   ```

4. **Custom Rate Limiting Middleware**
   - Build flexible rate limiting solutions:

   **Advanced Rate Limiting Middleware:**
   ```javascript
   // middleware/advanced-rate-limiter.js
   const { TokenBucket, DistributedTokenBucket } = require('../rate-limiters/token-bucket');
   const SlidingWindowRateLimiter = require('../rate-limiters/sliding-window');

   class AdvancedRateLimiter {
     constructor(redis) {
       this.redis = redis;
       this.rateLimiters = new Map();
   # ... (156 lines total, truncated)
   ```

5. **API Quota Management**
   - Implement comprehensive quota systems:

   **Quota Management System:**
   ```javascript
   // services/quota-manager.js
   class QuotaManager {
     constructor(redis, database) {
       this.redis = redis;
       this.database = database;
       this.quotaTypes = {
         'api_calls': { resetPeriod: 'monthly', defaultLimit: 10000 },
         'data_transfer': { resetPeriod: 'monthly', defaultLimit: 1073741824 }, // 1GB in bytes
   # ... (223 lines total, truncated)
   ```

6. **Rate Limiting for Different Services**
   - Implement service-specific rate limiting:

   **Database Rate Limiting:**
   ```javascript
   // rate-limiters/database-rate-limiter.js
   class DatabaseRateLimiter {
     constructor(redis, pool) {
       this.redis = redis;
       this.pool = pool;
       this.connectionLimiter = new Map();
       this.queryLimiter = new Map();
     }
   # ... (112 lines total, truncated)
   ```

   **File Upload Rate Limiting:**
   ```javascript
   // rate-limiters/upload-rate-limiter.js
   class UploadRateLimiter {
     constructor(redis) {
       this.redis = redis;
     }

     // Limit file upload size and frequency
     async checkUploadLimit(userId, fileSize, fileType) {
   # ... (138 lines total, truncated)
   ```

7. **Rate Limiting Dashboard and Analytics**
   - Monitor and analyze rate limiting effectiveness:

   **Rate Limiting Analytics:**
   ```javascript
   // analytics/rate-limit-analytics.js
   class RateLimitAnalytics {
     constructor(redis, database) {
       this.redis = redis;
       this.database = database;
     }

     async recordRateLimitHit(userId, endpoint, limitType, blocked) {
   # ... (191 lines total, truncated)
   ```

8. **Rate Limiting Configuration Management**
   - Dynamic rate limit configuration:

   **Configuration Manager:**
   ```javascript
   // config/rate-limit-config.js
   class RateLimitConfigManager {
     constructor(redis, database) {
       this.redis = redis;
       this.database = database;
       this.configCache = new Map();
       this.setupDefaultConfigs();
     }
   # ... (206 lines total, truncated)
   ```

9. **Testing Rate Limits**
   - Comprehensive rate limiting tests:

   **Rate Limiting Test Suite:**
   ```javascript
   // tests/rate-limiting.test.js
   const request = require('supertest');
   const app = require('../app');
   const Redis = require('ioredis');

   describe('Rate Limiting', () => {
     let redis;

   # ... (248 lines total, truncated)
   ```

10. **Production Monitoring and Alerting**
    - Monitor rate limiting effectiveness:

    **Rate Limiting Monitoring:**
    ```javascript
    // monitoring/rate-limit-monitor.js
    class RateLimitMonitor {
      constructor(redis, alertService) {
        this.redis = redis;
        this.alertService = alertService;
        this.thresholds = {
          highBlockRate: 0.15, // 15%
          highVolume: 10000,    // requests per minute
   # ... (244 lines total, truncated)
    ```
