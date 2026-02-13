# Database Migration Management

Create and manage database migration scripts for safe schema evolution.

## 1. Migration Strategy

- Analyze current database schema and target changes
- Plan migration strategy for zero-downtime deployments
- Define rollback procedures and data safety measures
- Assess migration complexity and potential risks
- Plan for data transformation and validation

## 2. Framework Selection

**Node.js**: Knex.js, TypeORM, Prisma, Sequelize
**Python**: Alembic, Django migrations, Flask-Migrate
**Java**: Flyway, Liquibase
**Go**: golang-migrate, goose
**Ruby**: ActiveRecord migrations

## 3. Migration File Structure

**SQL-based**:
```sql
-- +migrate Up
-- Description: Add user preferences table
-- Author: Developer Name
-- Date: 2024-01-15

CREATE TABLE user_preferences (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  preferences JSONB NOT NULL DEFAULT '{}',
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_user_preferences_user_id ON user_preferences(user_id);

-- +migrate Down
DROP TABLE IF EXISTS user_preferences;
```

## 4. Safe Schema Changes

**Column Addition**:
- Add new columns with safe defaults
- Use NULL defaults initially if possible
- Backfill data in separate migration
- Add constraints after data is populated

**Table Restructuring**:
- Create new table
- Dual-write to both tables
- Backfill historical data
- Switch reads to new table
- Remove old table

## 5. Data Migrations

- Validate data before transformation
- Transform in batches to avoid locks
- Log transformation progress
- Verify data integrity after migration
- Keep transformation logic in migration files

## 6. Testing Migrations

- Test against production data subset
- Verify migration up and down paths
- Check migration performance on large datasets
- Test concurrent application access during migration
- Validate data integrity post-migration

## 7. Production Safety

- Lock table size checks
- Execution time estimation
- Impact assessment on running applications
- Maintenance window planning
- Rollback plan verification

## 8. Migration Monitoring

- Track migration execution time
- Monitor database locks and blocking queries
- Alert on migration failures
- Log migration events for audit
- Track migration history

## 9. Version Control

- One migration per logical change
- Descriptive migration names with timestamps
- Never modify applied migrations
- Use sequential numbering or timestamps
- Tag releases with migration versions

## 10. CI/CD Integration

- Automated migration testing in CI
- Pre-deployment migration validation
- Automatic migration execution in deployment
- Migration rollback on deployment failure
- Post-deployment verification

See also: `design-database-schema.md`
