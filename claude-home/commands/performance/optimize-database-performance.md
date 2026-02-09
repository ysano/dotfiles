---
description: "Optimize database queries and performance"
---

## Instructions

1. **Database Performance Analysis**
   - Analyze current database performance and identify bottlenecks
   - Review slow query logs and execution plans
   - Assess database schema design and normalization
   - Evaluate indexing strategy and query patterns
   - Monitor database resource utilization (CPU, memory, I/O)

2. **Query Optimization**
   - Optimize slow queries and improve execution plans:

   **PostgreSQL Query Optimization:**
   ```sql
   -- Enable query logging for analysis
   ALTER SYSTEM SET log_statement = 'all';
   ALTER SYSTEM SET log_min_duration_statement = 1000; -- Log queries > 1 second
   SELECT pg_reload_conf();

   -- Analyze query performance
   EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) 
   SELECT u.id, u.name, COUNT(o.id) as order_count
   # ... (18 lines total, truncated)
   ```

   **MySQL Query Optimization:**
   ```sql
   -- Enable slow query log
   SET GLOBAL slow_query_log = 'ON';
   SET GLOBAL long_query_time = 1;
   SET GLOBAL log_queries_not_using_indexes = 'ON';

   -- Analyze query performance
   EXPLAIN FORMAT=JSON 
   SELECT p.*, c.name as category_name
   # ... (17 lines total, truncated)
   ```

3. **Index Strategy Optimization**
   - Design and implement optimal indexing strategy:

   **Index Analysis and Creation:**
   ```sql
   -- PostgreSQL index usage analysis
   SELECT 
     schemaname,
     tablename,
     indexname,
     idx_scan as index_scans,
     seq_scan as table_scans,
     idx_scan::float / (idx_scan + seq_scan + 1) as index_usage_ratio
   # ... (31 lines total, truncated)
   ```

   **Index Maintenance Scripts:**
   ```javascript
   // Node.js index analysis tool
   const { Pool } = require('pg');
   const pool = new Pool();

   class IndexAnalyzer {
     static async analyzeUnusedIndexes() {
       const query = `
         SELECT 
   # ... (43 lines total, truncated)
   ```

4. **Schema Design Optimization**
   - Optimize database schema for performance:

   **Normalization and Denormalization:**
   ```sql
   -- Denormalization example for read-heavy workloads
   -- Instead of joining multiple tables for product display
   CREATE TABLE product_display_cache AS
   SELECT 
     p.id,
     p.name,
     p.price,
     p.description,
   # ... (32 lines total, truncated)
   ```

   **Partitioning for Large Tables:**
   ```sql
   -- PostgreSQL table partitioning
   CREATE TABLE orders_partitioned (
     id SERIAL,
     user_id INTEGER,
     total_amount DECIMAL(10,2),
     created_at TIMESTAMP NOT NULL,
     status VARCHAR(50)
   ) PARTITION BY RANGE (created_at);
   # ... (30 lines total, truncated)
   ```

5. **Connection Pool Optimization**
   - Configure optimal database connection pooling:

   **Node.js Connection Pool Configuration:**
   ```javascript
   const { Pool } = require('pg');

   // Optimized connection pool configuration
   const pool = new Pool({
     user: process.env.DB_USER,
     host: process.env.DB_HOST,
     database: process.env.DB_NAME,
     password: process.env.DB_PASSWORD,
   # ... (41 lines total, truncated)
   ```

   **Database Connection Middleware:**
   ```javascript
   class DatabaseManager {
     static async executeQuery(query, params = []) {
       const client = await pool.connect();
       try {
         const start = Date.now();
         const result = await client.query(query, params);
         const duration = Date.now() - start;
         
   # ... (34 lines total, truncated)
   ```

6. **Query Result Caching**
   - Implement intelligent database result caching:

   ```javascript
   const Redis = require('redis');
   const redis = Redis.createClient();

   class QueryCache {
     static generateKey(query, params) {
       return `query:${Buffer.from(query + JSON.stringify(params)).toString('base64')}`;
     }

   # ... (42 lines total, truncated)
   ```

7. **Database Monitoring and Profiling**
   - Set up comprehensive database monitoring:

   **Performance Monitoring Script:**
   ```javascript
   class DatabaseMonitor {
     static async getPerformanceStats() {
       const queries = [
         {
           name: 'active_connections',
           query: 'SELECT count(*) FROM pg_stat_activity WHERE state = \'active\';'
         },
         {
   # ... (64 lines total, truncated)
   ```

8. **Read Replica and Load Balancing**
   - Configure read replicas for query distribution:

   ```javascript
   const { Pool } = require('pg');

   class DatabaseCluster {
     constructor() {
       this.writePool = new Pool({
         host: process.env.DB_WRITE_HOST,
         // ... write database config
       });
   # ... (51 lines total, truncated)
   ```

9. **Database Vacuum and Maintenance**
   - Implement automated database maintenance:

   **PostgreSQL Maintenance Scripts:**
   ```sql
   -- Automated vacuum and analyze
   CREATE OR REPLACE FUNCTION auto_vacuum_analyze()
   RETURNS void AS $$
   DECLARE
     rec RECORD;
   BEGIN
     FOR rec IN 
       SELECT schemaname, tablename 
   # ... (19 lines total, truncated)
   ```

   **Maintenance Monitoring:**
   ```javascript
   class MaintenanceMonitor {
     static async checkTableBloat() {
       const query = `
         SELECT 
           tablename,
           pg_size_pretty(pg_total_relation_size(tablename::regclass)) as size,
           n_dead_tup,
           n_live_tup,
   # ... (43 lines total, truncated)
   ```

10. **Performance Testing and Benchmarking**
    - Set up database performance testing:

    **Load Testing Script:**
    ```javascript
    const { Pool } = require('pg');
    const pool = new Pool();

    class DatabaseLoadTester {
      static async benchmarkQuery(query, params, iterations = 100) {
        const times = [];
        
        for (let i = 0; i < iterations; i++) {
   # ... (69 lines total, truncated)
    ```
