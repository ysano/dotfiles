---
description: "Implement GraphQL API endpoints"
---

## Instructions

1. **GraphQL Setup and Configuration**
   - Set up GraphQL server with Apollo Server or similar
   - Configure schema-first or code-first approach
   - Plan GraphQL architecture and data modeling
   - Set up development tools and introspection
   - Configure GraphQL playground and documentation

2. **Schema Definition and Type System**
   - Define comprehensive GraphQL schema:

   **Schema Definition (SDL):**
   ```graphql
   # schema/schema.graphql
   
   # Scalar types
   scalar DateTime
   scalar EmailAddress
   scalar PhoneNumber
   scalar JSON
   scalar Upload
   # ... (388 lines total, truncated)
   ```

3. **Resolver Implementation**
   - Implement comprehensive resolvers:

   **Main Resolvers:**
   ```javascript
   // resolvers/index.js
   const { GraphQLDateTime } = require('graphql-iso-date');
   const { GraphQLEmailAddress, GraphQLPhoneNumber } = require('graphql-scalars');
   const GraphQLJSON = require('graphql-type-json');
   const GraphQLUpload = require('graphql-upload/GraphQLUpload.js');

   const userResolvers = require('./userResolvers');
   const productResolvers = require('./productResolvers');
   # ... (46 lines total, truncated)
   ```

   **User Resolvers:**
   ```javascript
   // resolvers/userResolvers.js
   const { AuthenticationError, ForbiddenError, UserInputError } = require('apollo-server-express');
   const { withFilter } = require('graphql-subscriptions');
   const userService = require('../services/userService');
   const { requireAuth, requireRole } = require('../utils/authHelpers');
   const { createConnectionFromArray } = require('../utils/connectionHelpers');

   const userResolvers = {
   # ... (167 lines total, truncated)
   ```

4. **DataLoader for N+1 Problem**
   - Implement efficient data loading:

   **DataLoader Implementation:**
   ```javascript
   // dataLoaders/index.js
   const DataLoader = require('dataloader');
   const userService = require('../services/userService');
   const productService = require('../services/productService');
   const orderService = require('../services/orderService');

   class DataLoaders {
     constructor() {
   # ... (87 lines total, truncated)
   ```

5. **Authentication and Authorization**
   - Implement GraphQL-specific auth:

   **Auth Helpers:**
   ```javascript
   // utils/authHelpers.js
   const { AuthenticationError, ForbiddenError } = require('apollo-server-express');
   const jwt = require('jsonwebtoken');
   const userService = require('../services/userService');

   class GraphQLAuth {
     static async getUser(req) {
       const authHeader = req.headers.authorization;
   # ... (85 lines total, truncated)
   ```

6. **Real-time Subscriptions**
   - Implement GraphQL subscriptions:

   **Subscription Setup:**
   ```javascript
   // subscriptions/index.js
   const { PubSub } = require('graphql-subscriptions');
   const { RedisPubSub } = require('graphql-redis-subscriptions');
   const Redis = require('ioredis');

   // Use Redis for production, in-memory for development
   const createPubSub = () => {
     if (process.env.NODE_ENV === 'production') {
   # ... (105 lines total, truncated)
   ```

7. **Error Handling and Validation**
   - Implement comprehensive error handling:

   **Error Handling:**
   ```javascript
   // utils/errorHandling.js
   const { 
     ApolloError, 
     AuthenticationError, 
     ForbiddenError, 
     UserInputError 
   } = require('apollo-server-express');

   # ... (138 lines total, truncated)
   ```

8. **Performance Optimization**
   - Implement GraphQL performance optimizations:

   **Query Complexity and Depth Limiting:**
   ```javascript
   // utils/queryLimiting.js
   const depthLimit = require('graphql-depth-limit');
   const costAnalysis = require('graphql-query-complexity');

   class QueryLimiting {
     static createDepthLimit(maxDepth = 10) {
       return depthLimit(maxDepth, {
         ignoreIntrospection: true
   # ... (122 lines total, truncated)
   ```

9. **GraphQL Testing**
   - Implement comprehensive GraphQL testing:

   **GraphQL Test Suite:**
   ```javascript
   // tests/graphql/users.test.js
   const { createTestClient } = require('apollo-server-testing');
   const { gql } = require('apollo-server-express');
   const { createTestServer } = require('../helpers/testServer');
   const { createTestUser, getAuthToken } = require('../helpers/testHelpers');

   describe('User GraphQL API', () => {
     let server, query, mutate;
   # ... (329 lines total, truncated)
   ```

10. **Production Setup and Deployment**
    - Configure GraphQL for production:

    **Production Configuration:**
    ```javascript
    // server/apollo.js
    const { ApolloServer } = require('apollo-server-express');
    const { makeExecutableSchema } = require('@graphql-tools/schema');
    const { shield, rule, and, or } = require('graphql-shield');
    const depthLimit = require('graphql-depth-limit');
    const costAnalysis = require('graphql-query-complexity');

    const typeDefs = require('../schema');
   # ... (128 lines total, truncated)
    ```
