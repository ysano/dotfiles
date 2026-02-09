---
description: "Create and manage database migrations"
---

## Instructions

1. **Migration Strategy and Planning**
   - Analyze current database schema and target changes
   - Plan migration strategy for zero-downtime deployments
   - Define rollback procedures and data safety measures
   - Assess migration complexity and potential risks
   - Plan for data transformation and validation

2. **Migration Framework Setup**
   - Set up comprehensive migration framework:

   **Node.js Migration Framework:**
   ```javascript
   // migrations/migration-framework.js
   const fs = require('fs').promises;
   const path = require('path');
   const { Pool } = require('pg');

   class MigrationManager {
     constructor(databaseConfig) {
       this.pool = new Pool(databaseConfig);
   # ... (279 lines total, truncated)
   ```

3. **Migration File Templates**
   - Create standardized migration templates:

   **SQL Migration Template:**
   ```sql
   -- +migrate Up
   -- Migration: Add user preferences table
   -- Author: Developer Name
   -- Date: 2024-01-15
   -- Description: Create user_preferences table to store user-specific settings

   CREATE TABLE user_preferences (
     id BIGSERIAL PRIMARY KEY,
   # ... (33 lines total, truncated)
   ```

   **JavaScript Migration Template:**
   ```javascript
   // migrations/20240115120000_add_user_preferences.js
   const migration = {
     name: 'Add user preferences table',
     description: 'Create user_preferences table for storing user-specific settings',
     
     async up(client) {
       console.log('Creating user_preferences table...');
       
   # ... (43 lines total, truncated)
   ```

4. **Advanced Migration Patterns**
   - Implement complex migration scenarios:

   **Data Migration with Validation:**
   ```javascript
   // migrations/20240115130000_migrate_user_settings.js
   const migration = {
     name: 'Migrate user settings to new format',
     description: 'Transform legacy user_settings JSONB column to normalized user_preferences table',
     
     async up(client) {
       console.log('Starting user settings migration...');
       
   # ... (128 lines total, truncated)
   ```

5. **Schema Alteration Migrations**
   - Handle schema changes safely:

   **Safe Column Addition:**
   ```sql
   -- +migrate Up
   -- Migration: Add email verification tracking
   -- Safe column addition with default values

   -- Add new columns with safe defaults
   ALTER TABLE users 
   ADD COLUMN email_verification_token VARCHAR(255),
   ADD COLUMN email_verification_expires_at TIMESTAMP WITH TIME ZONE,
   # ... (32 lines total, truncated)
   ```

   **Safe Table Restructuring:**
   ```sql
   -- +migrate Up
   -- Migration: Split user addresses into separate table
   -- Zero-downtime table restructuring

   -- Step 1: Create new addresses table
   CREATE TABLE user_addresses (
     id BIGSERIAL PRIMARY KEY,
     user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
   # ... (97 lines total, truncated)
   ```

6. **Migration Testing Framework**
   - Test migrations thoroughly:

   **Migration Test Suite:**
   ```javascript
   // tests/migration-tests.js
   const { Pool } = require('pg');
   const MigrationManager = require('../migrations/migration-framework');

   class MigrationTester {
     constructor() {
       this.testDbConfig = {
         host: process.env.TEST_DB_HOST || 'localhost',
   # ... (232 lines total, truncated)
   ```

7. **Production Migration Safety**
   - Implement production-safe migration practices:

   **Safe Production Migration:**
   ```javascript
   // migrations/production-safety.js
   class ProductionMigrationSafety {
     static async validateProductionMigration(migrationFile, pool) {
       const safety = new ProductionMigrationSafety(pool);
       
       const checks = [
         safety.checkTableLocks.bind(safety),
         safety.checkDataSize.bind(safety),
   # ... (161 lines total, truncated)
   ```

8. **Migration Monitoring and Alerting**
   - Monitor migration execution:

   **Migration Monitoring:**
   ```javascript
   // migrations/migration-monitor.js
   class MigrationMonitor {
     constructor(alertService) {
       this.alertService = alertService;
       this.metrics = {
         executionTimes: [],
         errorCounts: {},
         successCounts: {}
   # ... (118 lines total, truncated)
   ```

9. **Migration CLI Tools**
   - Create comprehensive CLI interface:

   **Migration CLI:**
   ```javascript
   #!/usr/bin/env node
   // bin/migrate.js
   const yargs = require('yargs');
   const MigrationManager = require('../migrations/migration-framework');
   const MigrationTester = require('../tests/migration-tests');
   const MigrationMonitor = require('../migrations/migration-monitor');

   const dbConfig = {
   # ... (128 lines total, truncated)
   ```

10. **Production Deployment Integration**
    - Integrate with deployment pipelines:

    **CI/CD Integration:**
    ```yaml
    # .github/workflows/database-migration.yml
    name: Database Migration

    on:
      push:
        branches: [main]
        paths: ['migrations/**']
      
   # ... (81 lines total, truncated)
    ```
