# GraphQL API Implementation

Implement GraphQL APIs with proper schema design and resolver patterns.

## 1. GraphQL Setup

**Server Libraries**:
- Node.js: Apollo Server, GraphQL Yoga, Mercurius
- Python: Graphene, Ariadne, Strawberry
- Java: GraphQL Java, DGS Framework
- Go: gqlgen, graphql-go

**Approach**:
- Schema-first: Define schema in SDL, generate types
- Code-first: Define schema in code, generate SDL

## 2. Schema Definition

**Type System**:
```graphql
scalar DateTime
scalar EmailAddress
scalar JSON

type User {
  id: ID!
  email: EmailAddress!
  name: String!
  posts: [Post!]!
  createdAt: DateTime!
}

type Post {
  id: ID!
  title: String!
  content: String!
  author: User!
  published: Boolean!
}

type Query {
  user(id: ID!): User
  users(limit: Int, offset: Int): [User!]!
}

type Mutation {
  createUser(input: CreateUserInput!): User!
  updateUser(id: ID!, input: UpdateUserInput!): User!
}

input CreateUserInput {
  email: EmailAddress!
  name: String!
  password: String!
}
```

## 3. Resolver Implementation

**Resolver Pattern**:
- Resolvers for Query, Mutation, Subscription types
- Field resolvers for computed or lazy-loaded fields
- Keep resolvers thin, delegate to service layer
- Handle errors consistently

**DataLoader**: Use DataLoader to solve N+1 query problem by batching and caching database requests.

## 4. Authentication

**Context Setup**:
- Extract auth token from request headers
- Validate token and load user
- Attach user to context
- Use context in resolvers for authorization

**Authorization**:
- Check permissions in resolvers
- Use directive-based authorization
- Implement field-level permissions
- Return appropriate errors for unauthorized access

## 5. Subscriptions

**Real-time Updates**:
- Use WebSocket for subscription transport
- PubSub for event broadcasting
- Filter events based on user permissions
- Handle subscription lifecycle (connect, disconnect)

**Backends**:
- In-memory (development only)
- Redis (production, multi-server)
- Other message brokers (RabbitMQ, Kafka)

## 6. Error Handling

**Error Types**:
- User errors: Validation, authentication, authorization
- System errors: Database, external services, unexpected

**Error Format**:
```json
{
  "errors": [
    {
      "message": "User not found",
      "extensions": {
        "code": "NOT_FOUND",
        "userId": "123"
      },
      "path": ["user"],
      "locations": [{ "line": 2, "column": 3 }]
    }
  ]
}
```

## 7. Performance Optimization

**Query Complexity Limiting**:
- Limit query depth (max 10 levels)
- Calculate query cost based on fields
- Set cost limits per query
- Timeout long-running queries

**Caching**:
- Field-level caching for expensive operations
- CDN caching for public queries
- Client-side caching with Apollo Client

## 8. Schema Design Patterns

**Pagination**: Use Connection pattern (Relay-style) for cursor-based pagination.

**Versioning**: Use field deprecation instead of versioning:
```graphql
type User {
  oldField: String @deprecated(reason: "Use newField instead")
  newField: String
}
```

**Interfaces and Unions**: Use for polymorphic types.

## 9. Testing

**Test Layers**:
- Schema validation tests
- Resolver unit tests
- Integration tests with test client
- End-to-end tests with real GraphQL queries

**Tools**:
- GraphQL Testing Library
- Apollo Server Testing
- GraphQL Code Generator for typed tests

## 10. Production Setup

**Features**:
- Disable introspection in production
- Enable query persisting/whitelisting
- Set up APM monitoring
- Configure CORS properly
- Implement rate limiting
- Enable query logging for debugging

**Documentation**: Use GraphQL Playground or GraphiQL for interactive documentation.

See also: `rest-api-design.md`, `setup-rate-limiting.md`
