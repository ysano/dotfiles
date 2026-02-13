# REST API Design

Design RESTful APIs following best practices for scalability and maintainability.

## 1. API Strategy and Planning

- Analyze business requirements and define API scope
- Identify resources, entities, and their relationships
- Plan API versioning strategy and backward compatibility
- Define authentication and authorization requirements
- Plan for scalability, rate limiting, and performance

## 2. Resource Design

**URL Structure**:
- `/api/v1/users` - Collection endpoint
- `/api/v1/users/{id}` - Individual resource
- `/api/v1/users/{id}/orders` - Nested resources

**HTTP Methods**:
- GET: Retrieve resources (idempotent, cacheable)
- POST: Create resources
- PUT: Replace entire resource (idempotent)
- PATCH: Partial update
- DELETE: Remove resource (idempotent)

## 3. Request/Response Format

**Standard Response Structure**:
```json
{
  "status": "success",
  "data": { ... },
  "meta": {
    "pagination": { "page": 1, "limit": 20, "total": 100 }
  }
}
```

**Error Response**:
```json
{
  "status": "error",
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid input",
    "details": [
      { "field": "email", "message": "Invalid email format" }
    ]
  }
}
```

## 4. Data Validation

- Validate all input at API boundary
- Use schema validation libraries (Joi, Yup, Zod)
- Return detailed validation errors
- Sanitize input to prevent injection attacks
- Enforce data type and format constraints

## 5. Authentication and Authorization

**Authentication Methods**:
- JWT tokens for stateless auth
- OAuth 2.0 for third-party access
- API keys for service-to-service
- Session-based for traditional web apps

**Authorization**:
- Role-based access control (RBAC)
- Resource-based permissions
- Scope-based access for OAuth
- Principle of least privilege

## 6. API Versioning

**Strategies**:
- URL path: `/api/v1/users`, `/api/v2/users`
- Header: `Accept: application/vnd.api.v1+json`
- Query parameter: `/api/users?version=1`

**Best Practice**: Use URL path versioning for clarity.

## 7. Pagination and Filtering

**Pagination**:
- `?page=1&limit=20` (offset-based)
- `?cursor=abc123` (cursor-based for large datasets)
- Include pagination metadata in response

**Filtering**:
- `?status=active&role=admin`
- `?search=john`
- `?created_after=2024-01-01`

**Sorting**:
- `?sort=created_at&order=desc`
- `?sort=-created_at` (minus for descending)

## 8. Rate Limiting

- Implement per-user and per-endpoint rate limits
- Return rate limit headers:
  - `X-RateLimit-Limit`
  - `X-RateLimit-Remaining`
  - `X-RateLimit-Reset`
- Return 429 status when limit exceeded

See also: `setup-rate-limiting.md`

## 9. Documentation

**OpenAPI/Swagger**:
- Document all endpoints
- Include request/response examples
- Document error responses
- Provide authentication instructions
- Keep documentation in sync with implementation

## 10. Testing

- Unit tests for controller logic
- Integration tests for API endpoints
- Contract tests for API consumers
- Load tests for performance validation
- Security tests for vulnerability scanning

See also: `test` Skill, `security` Skill
