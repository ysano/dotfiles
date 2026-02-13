# Rate Limiting Implementation

Implement API rate limiting to protect services from abuse and ensure fair usage.

## 1. Strategy and Planning

- Analyze API endpoints and traffic patterns
- Define rate limiting policies for different user types and endpoints
- Plan for distributed rate limiting across multiple servers
- Consider different rate limiting algorithms
- Design rate limiting bypass mechanisms for trusted clients

## 2. Rate Limiting Algorithms

**Token Bucket**:
- Bucket capacity: Maximum burst size
- Refill rate: Tokens added per time period
- Allows bursts within capacity
- Good for APIs with variable load

**Sliding Window**:
- Count requests in rolling time window
- More accurate than fixed window
- Prevents boundary exploitation
- Higher memory usage

**Fixed Window**:
- Count requests per fixed time period
- Simple to implement
- Can allow 2x burst at window boundaries

**Leaky Bucket**:
- Process requests at constant rate
- Queue exceeding requests
- Smooths traffic spikes

## 3. Implementation Layers

**Application Level**:
- Express middleware
- Custom rate limiting logic
- Fine-grained control per endpoint

**Reverse Proxy**:
- Nginx limit_req module
- HAProxy stick tables
- Handles at network layer

**API Gateway**:
- Kong, AWS API Gateway, Azure API Management
- Centralized rate limiting
- Multiple backend protection

## 4. Storage Backend

**In-Memory** (single server):
- Simple, fast
- Not suitable for multi-server

**Redis** (distributed):
- Shared state across servers
- Atomic operations
- High performance

**Database** (persistent):
- Quota tracking
- Long-term rate limiting
- Slower than Redis

## 5. Rate Limit Headers

Return in every response:
```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 45
X-RateLimit-Reset: 1640000000
Retry-After: 3600
```

## 6. Response on Limit Exceeded

Status: 429 Too Many Requests

Response body:
```json
{
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Too many requests",
    "retryAfter": 3600
  }
}
```

## 7. Tiered Rate Limits

**Free tier**: 100 requests/hour
**Basic tier**: 1,000 requests/hour
**Premium tier**: 10,000 requests/hour
**Enterprise tier**: Unlimited or custom limits

Per-endpoint limits:
- Read operations: Higher limits
- Write operations: Lower limits
- Expensive operations: Strict limits

## 8. Quota Management

**Monthly/Daily Quotas**:
- Track total API calls per period
- Track data transfer quotas
- Track feature-specific quotas
- Reset on schedule
- Alert users before exhaustion

## 9. Bypass Mechanisms

**Trusted IPs**: Whitelist internal services
**Service accounts**: Higher limits for automation
**Emergency bypass**: Manual override capability
**Test mode**: Separate limits for testing

## 10. Monitoring and Analytics

**Metrics**:
- Rate limit hits per endpoint
- Rate limit violations per user
- Average request rate
- Peak request rate

**Alerting**:
- Alert on high rate limit hit rate
- Alert on potential abuse patterns
- Alert on rate limit configuration issues

**Dashboard**:
- Real-time rate limit status
- Per-user quota consumption
- Endpoint-level metrics
- Historical trends

See also: `rest-api-design.md`, `graphql-api.md`
