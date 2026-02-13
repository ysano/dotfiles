# Database Schema Design

Design efficient, scalable, and maintainable database schemas.

## 1. Requirements Analysis

- Analyze business requirements and data relationships
- Identify entities, attributes, and relationships
- Define data types, constraints, and validation rules
- Plan for scalability and future requirements
- Consider data access patterns and query requirements

## 2. Entity Relationship Design

Create normalized entity relationships:
- Primary keys: Use BIGSERIAL or UUID
- Foreign keys: Enforce referential integrity
- Unique constraints: Prevent duplicates
- Check constraints: Validate data at database level
- NOT NULL constraints: Ensure data completeness

## 3. Indexing Strategy

**Types of indexes**:
- B-tree: Default, good for equality and range queries
- Hash: Fast equality lookups only
- GiST/GIN: Full-text search and JSON queries
- Partial: Index subset of rows
- Composite: Multi-column indexes for common query patterns

**Index placement**:
- Foreign keys for join performance
- Columns in WHERE clauses
- Columns in ORDER BY clauses
- Columns in JOIN conditions

## 4. Data Types Selection

- Use appropriate precision (BIGINT vs INT, DECIMAL vs FLOAT)
- Use VARCHAR with appropriate limits, not unlimited TEXT
- Use TIMESTAMP WITH TIME ZONE for temporal data
- Use JSONB (not JSON) for flexible attributes
- Use ENUM or CHECK constraints for fixed values

## 5. Performance Patterns

**Partitioning**: Split large tables by date, range, or hash
**Denormalization**: Duplicate data strategically for read performance
**Materialized Views**: Pre-compute expensive aggregations
**Read Replicas**: Separate read and write workloads

## 6. Data Integrity

**Constraints**:
- Primary key constraints
- Foreign key constraints with ON DELETE/UPDATE actions
- Unique constraints for natural keys
- Check constraints for business rules
- NOT NULL constraints for required fields

**Triggers**: Use sparingly for:
- Audit logging
- Derived field updates
- Complex validation

## 7. Audit and Versioning

**Audit columns**:
- created_at, updated_at timestamps
- created_by, updated_by user tracking
- deleted_at for soft deletes
- version for optimistic locking

**Audit tables**:
- Separate audit log table
- Trigger-based or application-based logging
- Store old and new values
- Track operation type (INSERT/UPDATE/DELETE)

## 8. Security

**Row Level Security (RLS)**:
- Define policies for data access
- Filter rows based on user context
- Enforce at database level

**Sensitive Data**:
- Encrypt sensitive columns
- Hash passwords with proper algorithms
- Use separate credentials for different access levels
- Implement column-level permissions

## 9. Schema Documentation

Add comments to schema objects:
```sql
COMMENT ON TABLE users IS 'User accounts and authentication';
COMMENT ON COLUMN users.email IS 'Unique email for authentication';
```

Maintain ER diagrams and data dictionaries.

## 10. Schema Testing

- Test data integrity constraints
- Validate foreign key relationships
- Test index performance
- Verify trigger behavior
- Load test with realistic data volumes

See also: `database-migrations.md`, `performance` Skill `references/database-optimization.md`
