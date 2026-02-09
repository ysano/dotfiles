---
description: "Design optimized database schemas"
---

## Instructions

1. **Requirements Analysis and Data Modeling**
   - Analyze business requirements and data relationships
   - Identify entities, attributes, and relationships
   - Define data types, constraints, and validation rules
   - Plan for scalability and future requirements
   - Consider data access patterns and query requirements

2. **Entity Relationship Design**
   - Create comprehensive entity relationship diagrams:

   **User Management Schema:**
   ```sql
   -- Users table with proper indexing and constraints
   CREATE TABLE users (
     id BIGSERIAL PRIMARY KEY,
     email VARCHAR(255) UNIQUE NOT NULL,
     username VARCHAR(50) UNIQUE NOT NULL,
     password_hash VARCHAR(255) NOT NULL,
     first_name VARCHAR(100) NOT NULL,
     last_name VARCHAR(100) NOT NULL,
   # ... (58 lines total, truncated)
   ```

   **E-commerce Schema Example:**
   ```sql
   -- Categories with hierarchical structure
   CREATE TABLE categories (
     id SERIAL PRIMARY KEY,
     name VARCHAR(255) NOT NULL,
     slug VARCHAR(255) UNIQUE NOT NULL,
     description TEXT,
     parent_id INTEGER REFERENCES categories(id),
     sort_order INTEGER DEFAULT 0,
   # ... (125 lines total, truncated)
   ```

3. **Advanced Schema Patterns**
   - Implement complex data patterns:

   **Audit Trail Pattern:**
   ```sql
   -- Generic audit trail for tracking all changes
   CREATE TABLE audit_log (
     id BIGSERIAL PRIMARY KEY,
     table_name VARCHAR(255) NOT NULL,
     record_id BIGINT NOT NULL,
     operation audit_operation NOT NULL,
     old_values JSONB,
     new_values JSONB,
   # ... (57 lines total, truncated)
   ```

   **Soft Delete Pattern:**
   ```sql
   -- Add soft delete to any table
   ALTER TABLE users ADD COLUMN deleted_at TIMESTAMP WITH TIME ZONE;
   ALTER TABLE products ADD COLUMN deleted_at TIMESTAMP WITH TIME ZONE;

   -- Create views that exclude soft-deleted records
   CREATE VIEW active_users AS
   SELECT * FROM users WHERE deleted_at IS NULL;

   # ... (28 lines total, truncated)
   ```

4. **Performance Optimization Schema Design**
   - Design for optimal query performance:

   **Strategic Indexing:**
   ```sql
   -- Single column indexes for frequently queried fields
   CREATE INDEX CONCURRENTLY idx_users_email ON users(email);
   CREATE INDEX CONCURRENTLY idx_users_username ON users(username);
   CREATE INDEX CONCURRENTLY idx_users_status ON users(status) WHERE status != 'active';
   CREATE INDEX CONCURRENTLY idx_users_created_at ON users(created_at);

   -- Composite indexes for common query patterns
   CREATE INDEX CONCURRENTLY idx_products_category_status 
   # ... (34 lines total, truncated)
   ```

   **Partitioning Strategy:**
   ```sql
   -- Partition large tables by date for better performance
   CREATE TABLE orders_partitioned (
     LIKE orders INCLUDING ALL
   ) PARTITION BY RANGE (created_at);

   -- Create monthly partitions
   CREATE TABLE orders_2024_01 PARTITION OF orders_partitioned
   FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');
   # ... (38 lines total, truncated)
   ```

5. **Data Integrity and Constraints**
   - Implement comprehensive data validation:

   **Advanced Constraints:**
   ```sql
   -- Complex check constraints
   ALTER TABLE products ADD CONSTRAINT products_price_logic 
   CHECK (
     CASE 
       WHEN compare_price IS NOT NULL THEN price <= compare_price
       ELSE true
     END
   );
   # ... (57 lines total, truncated)
   ```

6. **Temporal Data and Versioning**
   - Handle time-based data requirements:

   **Temporal Tables:**
   ```sql
   -- Product price history tracking
   CREATE TABLE product_price_history (
     id BIGSERIAL PRIMARY KEY,
     product_id BIGINT REFERENCES products(id) ON DELETE CASCADE,
     price DECIMAL(10,2) NOT NULL,
     compare_price DECIMAL(10,2),
     effective_from TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
     effective_to TIMESTAMP WITH TIME ZONE,
   # ... (60 lines total, truncated)
   ```

7. **JSON/NoSQL Integration**
   - Leverage JSON columns for flexible data:

   **JSONB Schema Design:**
   ```sql
   -- Flexible product attributes using JSONB
   CREATE TABLE product_attributes (
     product_id BIGINT REFERENCES products(id) ON DELETE CASCADE,
     attributes JSONB NOT NULL DEFAULT '{}',
     created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
     updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
     
     PRIMARY KEY (product_id)
   # ... (61 lines total, truncated)
   ```

8. **Database Security Schema**
   - Implement security at the schema level:

   **Row Level Security:**
   ```sql
   -- Enable RLS on sensitive tables
   ALTER TABLE orders ENABLE ROW LEVEL SECURITY;
   ALTER TABLE user_profiles ENABLE ROW LEVEL SECURITY;

   -- Create policies for data access
   CREATE POLICY orders_user_access ON orders
   FOR ALL TO authenticated_users
   USING (user_id = current_user_id());
   # ... (50 lines total, truncated)
   ```

9. **Schema Documentation and Maintenance**
   - Document and maintain schema design:

   **Database Documentation:**
   ```sql
   -- Add comments to tables and columns
   COMMENT ON TABLE users IS 'User accounts and authentication information';
   COMMENT ON COLUMN users.email IS 'Unique email address for user authentication';
   COMMENT ON COLUMN users.status IS 'Current status of user account (active, inactive, suspended, pending_verification)';
   COMMENT ON COLUMN users.email_verified IS 'Whether the user has verified their email address';

   COMMENT ON TABLE products IS 'Product catalog with inventory and pricing information';
   COMMENT ON COLUMN products.search_vector IS 'Full-text search vector generated from name, description, and SKU';
   # ... (26 lines total, truncated)
   ```

10. **Schema Testing and Validation**
    - Implement schema testing procedures:

    **Schema Validation Tests:**
    ```sql
    -- Test data integrity constraints
    DO $$
    DECLARE
      test_result BOOLEAN;
    BEGIN
      -- Test email validation
      BEGIN
        INSERT INTO users (email, username, password_hash, first_name, last_name)
   # ... (76 lines total, truncated)
    ```
