---
description: "Design RESTful API architecture"
---

## Instructions

1. **API Design Strategy and Planning**
   - Analyze business requirements and define API scope
   - Identify resources, entities, and their relationships
   - Plan API versioning strategy and backward compatibility
   - Define authentication and authorization requirements
   - Plan for scalability, rate limiting, and performance

2. **RESTful Resource Design**
   - Design RESTful endpoints following REST principles:

   **Express.js API Structure:**
   ```javascript
   // routes/api/v1/index.js
   const express = require('express');
   const router = express.Router();

   // Resource-based routing structure
   const userRoutes = require('./users');
   const productRoutes = require('./products');
   const orderRoutes = require('./orders');
   # ... (79 lines total, truncated)
   ```

3. **Request/Response Data Models**
   - Define comprehensive data models and validation:

   **Data Validation with Joi:**
   ```javascript
   // validations/userValidation.js
   const Joi = require('joi');

   const userSchema = {
     create: Joi.object({
       email: Joi.string().email().required(),
       password: Joi.string().min(8).pattern(/^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/).required(),
       firstName: Joi.string().trim().min(1).max(100).required(),
   # ... (101 lines total, truncated)
   ```

4. **Controller Implementation**
   - Implement robust controller logic:

   **User Controller Example:**
   ```javascript
   // controllers/userController.js
   const userService = require('../services/userService');
   const { ApiError, ApiResponse } = require('../utils/apiResponse');

   class UserController {
     async listUsers(req, res, next) {
       try {
         const { page, limit, sort, order, search, status, role } = req.query;
   # ... (168 lines total, truncated)
   ```

5. **API Response Standardization**
   - Implement consistent response formats:

   **API Response Utilities:**
   ```javascript
   // utils/apiResponse.js
   class ApiResponse {
     constructor(status, message, data = null, meta = null) {
       this.status = status;
       this.message = message;
       this.timestamp = new Date().toISOString();
       
       if (data !== null) {
   # ... (115 lines total, truncated)
   ```

6. **Authentication and Authorization**
   - Implement comprehensive auth system:

   **JWT Authentication Middleware:**
   ```javascript
   // middleware/auth.js
   const jwt = require('jsonwebtoken');
   const { ApiError } = require('../utils/apiResponse');
   const userService = require('../services/userService');

   class AuthMiddleware {
     static async authenticate(req, res, next) {
       try {
   # ... (134 lines total, truncated)
   ```

7. **API Documentation with OpenAPI/Swagger**
   - Generate comprehensive API documentation:

   **Swagger Configuration:**
   ```javascript
   // swagger/swagger.js
   const swaggerJsdoc = require('swagger-jsdoc');
   const swaggerUi = require('swagger-ui-express');

   const options = {
     definition: {
       openapi: '3.0.0',
       info: {
   # ... (211 lines total, truncated)
   ```

   **Controller Documentation:**
   ```javascript
   // Add to userController.js
   /**
    * @swagger
    * /api/v1/users:
    *   get:
    *     summary: List all users
    *     tags: [Users]
    *     security:
   # ... (116 lines total, truncated)
   ```

8. **API Testing and Quality Assurance**
   - Implement comprehensive API testing:

   **API Test Suite:**
   ```javascript
   // tests/api/users.test.js
   const request = require('supertest');
   const app = require('../../app');
   const { setupTestDb, teardownTestDb, createTestUser, getAuthToken } = require('../helpers/testHelpers');

   describe('Users API', () => {
     let authToken;
     let testUser;
   # ... (181 lines total, truncated)
   ```

9. **API Versioning Strategy**
   - Implement flexible API versioning:

   **Version Management:**
   ```javascript
   // middleware/versioning.js
   class ApiVersioning {
     static extractVersion(req) {
       // Support multiple versioning strategies
       
       // 1. URL path versioning (preferred)
       const pathVersion = req.path.match(/^\/api\/v(\d+)/);
       if (pathVersion) {
   # ... (88 lines total, truncated)
   ```

10. **Production Monitoring and Analytics**
    - Implement API monitoring and analytics:

    **API Analytics Middleware:**
    ```javascript
    // middleware/analytics.js
    const prometheus = require('prom-client');

    class ApiAnalytics {
      constructor() {
        this.setupMetrics();
      }

   # ... (98 lines total, truncated)
    ```
