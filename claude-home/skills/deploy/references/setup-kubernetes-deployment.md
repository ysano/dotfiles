# Setup Kubernetes Deployment - Kubernetes デプロイ設定

Kubernetes クラスタへのアプリケーションデプロイを設定する手順。

## 1. クラスタアーキテクチャ設計

Kubernetes クラスタの構成を設計。

- リソース要件 (CPU, メモリ, ストレージ, ネットワーク)
- Namespace 戦略 (環境分離、チーム分離)
- 高可用性要件 (レプリカ数、ゾーン分散)
- スケーリング戦略 (HPA, VPA, Cluster Autoscaler)

## 2. Deployment マニフェスト作成

アプリケーションの Deployment を定義。

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  namespace: production
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
    spec:
      containers:
      - name: myapp
        image: registry.example.com/myapp:v1.0.0
        ports:
        - containerPort: 3000
        resources:
          requests:
            cpu: "100m"
            memory: "128Mi"
          limits:
            cpu: "500m"
            memory: "512Mi"
        livenessProbe:
          httpGet:
            path: /health
            port: 3000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 3000
          initialDelaySeconds: 5
          periodSeconds: 5
```

## 3. Service 作成

アプリケーションへのネットワークアクセスを設定。

```yaml
apiVersion: v1
kind: Service
metadata:
  name: myapp-service
  namespace: production
spec:
  selector:
    app: myapp
  ports:
  - protocol: TCP
    port: 80
    targetPort: 3000
  type: ClusterIP
```

## 4. ConfigMap と Secret 設定

設定とシークレットを管理。

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: myapp-config
data:
  LOG_LEVEL: "info"
  API_TIMEOUT: "30"
---
apiVersion: v1
kind: Secret
metadata:
  name: myapp-secrets
type: Opaque
data:
  DATABASE_PASSWORD: base64encodedpassword
  API_KEY: base64encodedkey
```

**参照方法**:
```yaml
envFrom:
- configMapRef:
    name: myapp-config
- secretRef:
    name: myapp-secrets
```

## 5. Ingress 設定

外部トラフィックルーティングを設定。

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: myapp-ingress
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - myapp.example.com
    secretName: myapp-tls
  rules:
  - host: myapp.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: myapp-service
            port:
              number: 80
```

## 6. HorizontalPodAutoscaler 設定

負荷に応じた自動スケーリングを設定。

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: myapp-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: myapp
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
```

## 7. PersistentVolume 設定

データ永続化が必要な場合。

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: myapp-data
spec:
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 10Gi
  storageClassName: standard
```

## 8. NetworkPolicy 設定

セキュリティのためネットワーク分離。

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: myapp-netpol
spec:
  podSelector:
    matchLabels:
      app: myapp
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          role: frontend
    ports:
    - protocol: TCP
      port: 3000
```

## 9. デプロイ実行

マニフェストを適用してデプロイ。

```bash
# 設定適用
kubectl apply -f k8s/

# デプロイ状態確認
kubectl rollout status deployment/myapp -n production

# Pod 確認
kubectl get pods -n production -l app=myapp

# ログ確認
kubectl logs -f deployment/myapp -n production
```

## 10. 監視とオペレーション

デプロイ後の運用設定。

- Prometheus / Grafana で監視
- ログ集約 (Loki, Elasticsearch)
- アラート設定
- ダッシュボード作成
- バックアップ戦略

**ロールバック手順**:
```bash
kubectl rollout undo deployment/myapp -n production
kubectl rollout status deployment/myapp -n production
```

## 関連リファレンス

- `containerize-application.md` - コンテナイメージ準備
- `rollback-deploy.md` - ロールバック詳細手順
- `ci-setup.md` - CI/CD 統合
