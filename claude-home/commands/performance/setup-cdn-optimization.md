---
description: "Configure CDN for optimal delivery"
---

## Instructions

1. **CDN Strategy and Provider Selection**
   - Analyze application traffic patterns and global user distribution
   - Evaluate CDN providers (CloudFlare, AWS CloudFront, Fastly, KeyCDN)
   - Assess content types and caching requirements
   - Plan CDN architecture and edge location strategy
   - Define performance and cost optimization goals

2. **CDN Configuration and Setup**
   - Configure CDN with optimal settings:

   **CloudFlare Configuration:**
   ```javascript
   // Cloudflare Page Rules via API
   const cloudflare = require('cloudflare');
   const cf = new cloudflare({
     email: process.env.CLOUDFLARE_EMAIL,
     key: process.env.CLOUDFLARE_API_KEY
   });

   const pageRules = [
   # ... (30 lines total, truncated)
   ```

   **AWS CloudFront Distribution:**
   ```yaml
   # cloudformation-cdn.yaml
   AWSTemplateFormatVersion: '2010-09-09'
   Resources:
     CloudFrontDistribution:
       Type: AWS::CloudFront::Distribution
       Properties:
         DistributionConfig:
           Origins:
   # ... (41 lines total, truncated)
   ```

3. **Static Asset Optimization**
   - Optimize assets for CDN delivery:

   **Asset Build Process:**
   ```javascript
   // webpack.config.js - CDN optimization
   const path = require('path');
   const { CleanWebpackPlugin } = require('clean-webpack-plugin');
   const MiniCssExtractPlugin = require('mini-css-extract-plugin');

   module.exports = {
     output: {
       path: path.resolve(__dirname, 'dist'),
   # ... (56 lines total, truncated)
   ```

   **Next.js CDN Configuration:**
   ```javascript
   // next.config.js
   const withOptimizedImages = require('next-optimized-images');

   module.exports = withOptimizedImages({
     assetPrefix: process.env.CDN_URL || '',
     
     images: {
       domains: ['cdn.example.com'],
   # ... (28 lines total, truncated)
   ```

4. **Compression and Optimization**
   - Configure optimal compression settings:

   **Gzip/Brotli Compression:**
   ```javascript
   // Express.js compression middleware
   const compression = require('compression');
   const express = require('express');
   const app = express();

   // Advanced compression configuration
   app.use(compression({
     level: 6, // Compression level (1-9)
   # ... (36 lines total, truncated)
   ```

   **Build-time Compression:**
   ```javascript
   // compression-plugin.js
   const CompressionPlugin = require('compression-webpack-plugin');
   const BrotliPlugin = require('brotli-webpack-plugin');

   module.exports = {
     plugins: [
       // Gzip compression
       new CompressionPlugin({
   # ... (23 lines total, truncated)
   ```

5. **Cache Headers and Policies**
   - Configure optimal caching strategies:

   **Smart Cache Headers:**
   ```javascript
   // cache-control.js
   class CacheControlManager {
     static getCacheHeaders(filePath, fileType) {
       const cacheStrategies = {
         // Long-term caching for versioned assets
         versioned: {
           'Cache-Control': 'public, max-age=31536000, immutable',
           'Expires': new Date(Date.now() + 31536000000).toUTCString(),
   # ... (61 lines total, truncated)
   ```

6. **Image Optimization and Delivery**
   - Implement advanced image optimization:

   **Responsive Image Delivery:**
   ```javascript
   // image-optimization.js
   const sharp = require('sharp');
   const fs = require('fs').promises;

   class ImageOptimizer {
     static async generateResponsiveImages(inputPath, outputDir) {
       const sizes = [
         { width: 320, suffix: 'sm' },
   # ... (65 lines total, truncated)
   ```

7. **CDN Purging and Cache Invalidation**
   - Implement intelligent cache invalidation:

   **CloudFlare Cache Purging:**
   ```javascript
   // cdn-purge.js
   const cloudflare = require('cloudflare');

   class CDNManager {
     constructor() {
       this.cf = new cloudflare({
         email: process.env.CLOUDFLARE_EMAIL,
         key: process.env.CLOUDFLARE_API_KEY
   # ... (66 lines total, truncated)
   ```

8. **Performance Monitoring and Analytics**
   - Set up CDN performance monitoring:

   **CDN Performance Tracking:**
   ```javascript
   // cdn-analytics.js
   class CDNAnalytics {
     static async getCDNMetrics() {
       const metrics = {
         cacheHitRatio: await this.getCacheHitRatio(),
         bandwidth: await this.getBandwidthUsage(),
         responseTime: await this.getResponseTimes(),
         errorRate: await this.getErrorRate(),
   # ... (63 lines total, truncated)
   ```

9. **Security and Access Control**
   - Configure CDN security features:

   **CDN Security Configuration:**
   ```javascript
   // cdn-security.js
   class CDNSecurity {
     static setupSecurityHeaders() {
       return {
         'Strict-Transport-Security': 'max-age=31536000; includeSubDomains; preload',
         'X-Content-Type-Options': 'nosniff',
         'X-Frame-Options': 'DENY',
         'X-XSS-Protection': '1; mode=block',
   # ... (43 lines total, truncated)
   ```

10. **Cost Optimization and Monitoring**
    - Implement CDN cost optimization:

    **Cost Monitoring:**
    ```javascript
    // cdn-cost-optimization.js
    class CDNCostOptimizer {
      static async analyzeUsage() {
        const usage = await this.getCDNUsage();
        const recommendations = [];

        // Analyze bandwidth usage by file type
        if (usage.images > usage.total * 0.6) {
   # ... (55 lines total, truncated)
    ```
