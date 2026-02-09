---
description: "Reduce and optimize bundle sizes"
---

## Instructions

1. **Bundle Analysis and Assessment**
   - Analyze current bundle size and composition using webpack-bundle-analyzer or similar
   - Identify large dependencies and unused code
   - Assess current build configuration and optimization settings
   - Create baseline measurements for optimization tracking
   - Document current performance metrics and loading times

2. **Build Tool Configuration**
   - Configure build tool optimization settings:

   **Webpack Configuration:**
   ```javascript
   // webpack.config.js
   const path = require('path');
   const { BundleAnalyzerPlugin } = require('webpack-bundle-analyzer');
// ... (32 lines truncated)
   ```

   **Vite Configuration:**
   ```javascript
   // vite.config.js
   import { defineConfig } from 'vite';
   import { visualizer } from 'rollup-plugin-visualizer';
// ... (21 lines truncated)
   ```

3. **Code Splitting and Lazy Loading**
   - Implement route-based code splitting:

   **React Route Splitting:**
   ```javascript
   import { lazy, Suspense } from 'react';
   import { Routes, Route } from 'react-router-dom';

// ... (16 lines truncated)
   ```

   **Dynamic Imports:**
   ```javascript
   // Lazy load heavy components
   const HeavyComponent = lazy(() => 
     import('./HeavyComponent').then(module => ({
// ... (12 lines truncated)
   ```

4. **Tree Shaking and Dead Code Elimination**
   - Configure tree shaking for optimal dead code elimination:

   **Package.json Configuration:**
   ```json
   {
     "sideEffects": false,
     "exports": {
// ... (7 lines truncated)
   ```

   **Import Optimization:**
   ```javascript
   // Instead of importing entire library
   // import * as _ from 'lodash';

// ... (16 lines truncated)
   ```

5. **Dependency Optimization**
   - Analyze and optimize dependencies:

   **Package Analysis Script:**
   ```javascript
   // scripts/analyze-deps.js
   const fs = require('fs');
   const path = require('path');
// ... (27 lines truncated)
   ```

6. **Asset Optimization**
   - Optimize static assets and media files:

   **Image Optimization:**
   ```javascript
   // webpack.config.js
   module.exports = {
     module: {
// ... (25 lines truncated)
   ```

7. **Module Federation and Micro-frontends**
   - Implement module federation for large applications:

   **Module Federation Setup:**
   ```javascript
   // webpack.config.js
   const ModuleFederationPlugin = require('@module-federation/webpack');

// ... (16 lines truncated)
   ```

8. **Performance Monitoring and Measurement**
   - Set up bundle size monitoring:

   **Bundle Size Monitoring:**
   ```javascript
   // scripts/bundle-monitor.js
   const fs = require('fs');
   const path = require('path');
// ... (22 lines truncated)
   ```

9. **Progressive Loading Strategies**
   - Implement progressive loading and resource hints:

   **Resource Hints:**
   ```html
   <!-- Preload critical resources -->
   <link rel="preload" href="/fonts/main.woff2" as="font" type="font/woff2" crossorigin>
   <link rel="preload" href="/critical.css" as="style">
// ... (8 lines truncated)
   ```

   **Intersection Observer for Lazy Loading:**
   ```javascript
   // utils/lazyLoad.js
   export function lazyLoadComponent(importFunc) {
     return lazy(() => {
// ... (17 lines truncated)
   ```

10. **Validation and Continuous Monitoring**
    - Set up automated bundle size validation:

    **CI/CD Bundle Size Check:**
    ```yaml
    # .github/workflows/bundle-size.yml
    name: Bundle Size Check
    on: [pull_request]
// ... (19 lines truncated)
    ```

    **Bundle Size Threshold Check:**
    ```javascript
    // scripts/bundle-size-check.js
    const fs = require('fs');
    const path = require('path');
// ... (32 lines truncated)
    ```
