---
description: "Design and implement caching solutions"
---

## Instructions

1. **Caching Strategy Analysis**
   - Analyze application architecture and identify caching opportunities
   - Assess current performance bottlenecks and data access patterns
   - Define caching requirements (TTL, invalidation, consistency)
   - Plan multi-layer caching architecture (browser, CDN, application, database)
   - Evaluate caching technologies and storage solutions

2. **Browser and Client-Side Caching**
   - Configure HTTP caching headers and cache policies:

   **HTTP Cache Headers:**
   ```javascript
   // Express.js middleware
   app.use((req, res, next) => {
     // Static assets with long-term caching
     if (req.url.match(/\.(js|css|png|jpg|jpeg|gif|ico|svg)$/)) {
       res.setHeader('Cache-Control', 'public, max-age=31536000'); // 1 year
       res.setHeader('ETag', generateETag(req.url));
     }
     
   # ... (15 lines total, truncated)
   ```

   **Service Worker Caching:**
   ```javascript
   // sw.js - Service Worker
   const CACHE_NAME = 'app-cache-v1';
   const urlsToCache = [
     '/',
     '/static/js/bundle.js',
     '/static/css/main.css',
   ];

   # ... (24 lines total, truncated)
   ```

3. **Application-Level Caching**
   - Implement in-memory and distributed caching:

   **Node.js Memory Cache:**
   ```javascript
   const NodeCache = require('node-cache');
   const cache = new NodeCache({ stdTTL: 600 }); // 10 minutes default TTL

   class CacheService {
     static get(key) {
       return cache.get(key);
     }

   # ... (44 lines total, truncated)
   ```

   **Redis Distributed Cache:**
   ```javascript
   const redis = require('redis');
   const client = redis.createClient({
     host: process.env.REDIS_HOST || 'localhost',
     port: process.env.REDIS_PORT || 6379,
   });

   class RedisCache {
     static async get(key) {
   # ... (56 lines total, truncated)
   ```

4. **Database Query Caching**
   - Implement database-level caching strategies:

   **PostgreSQL Query Caching:**
   ```javascript
   const { Pool } = require('pg');
   const pool = new Pool();

   class DatabaseCache {
     static async cachedQuery(sql, params = [], ttl = 300) {
       const cacheKey = `query:${Buffer.from(sql + JSON.stringify(params)).toString('base64')}`;
       
       // Try cache first
   # ... (36 lines total, truncated)
   ```

   **MongoDB Caching with Mongoose:**
   ```javascript
   const mongoose = require('mongoose');

   // Mongoose query caching plugin
   function cachePlugin(schema) {
     schema.add({
       cacheKey: { type: String, index: true },
       cachedAt: { type: Date },
     });
   # ... (41 lines total, truncated)
   ```

5. **API Response Caching**
   - Implement comprehensive API caching:

   **Express Cache Middleware:**
   ```javascript
   function cacheMiddleware(ttl = 300) {
     return async (req, res, next) => {
       // Only cache GET requests
       if (req.method !== 'GET') {
         return next();
       }

       const cacheKey = `api:${req.originalUrl}`;
   # ... (30 lines total, truncated)
   ```

   **GraphQL Query Caching:**
   ```javascript
   const { ApolloServer } = require('apollo-server-express');
   const { ResponseCache } = require('apollo-server-plugin-response-cache');

   const server = new ApolloServer({
     typeDefs,
     resolvers,
     plugins: [
       ResponseCache({
   # ... (33 lines total, truncated)
   ```

6. **Cache Invalidation Strategies**
   - Implement intelligent cache invalidation:

   **Event-Driven Cache Invalidation:**
   ```javascript
   const EventEmitter = require('events');
   const cacheInvalidator = new EventEmitter();

   class CacheInvalidator {
     static invalidateUser(userId) {
       const patterns = [
         `user:${userId}*`,
         `api:/api/users/${userId}*`,
   # ... (41 lines total, truncated)
   ```

7. **Frontend Caching Strategies**
   - Implement client-side caching:

   **React Query Caching:**
   ```javascript
   import { QueryClient, QueryClientProvider, useQuery } from 'react-query';

   const queryClient = new QueryClient({
     defaultOptions: {
       queries: {
         staleTime: 5 * 60 * 1000, // 5 minutes
         cacheTime: 10 * 60 * 1000, // 10 minutes
         retry: 3,
   # ... (34 lines total, truncated)
   ```

   **Local Storage Caching:**
   ```javascript
   class LocalStorageCache {
     static set(key, value, ttl = 3600000) { // 1 hour default
       const item = {
         value,
         expiry: Date.now() + ttl,
       };
       localStorage.setItem(key, JSON.stringify(item));
     }
   # ... (30 lines total, truncated)
   ```

8. **Cache Monitoring and Analytics**
   - Set up cache performance monitoring:

   **Cache Metrics Collection:**
   ```javascript
   class CacheMetrics {
     static hits = 0;
     static misses = 0;
     static errors = 0;

     static recordHit() {
       this.hits++;
     }
   # ... (57 lines total, truncated)
   ```

9. **Cache Warming and Preloading**
   - Implement cache warming strategies:

   **Scheduled Cache Warming:**
   ```javascript
   const cron = require('node-cron');

   class CacheWarmer {
     static async warmPopularData() {
       console.log('Starting cache warming...');
       
       // Warm popular products
       const popularProducts = await DatabaseCache.cachedQuery(
   # ... (37 lines total, truncated)
   ```

10. **Testing and Validation**
    - Set up cache testing and validation:

    **Cache Testing:**
    ```javascript
    // tests/cache.test.js
    const request = require('supertest');
    const app = require('../app');

    describe('Cache Performance', () => {
      test('should cache API responses', async () => {
        // First request - should miss cache
        const start1 = Date.now();
   # ... (34 lines total, truncated)
    ```
