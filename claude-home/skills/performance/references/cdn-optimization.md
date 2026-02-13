# CDN 最適化手順

CDN 設定とコンテンツ配信を以下の 10 ステップで最適化する。

## 1. CDN 戦略・プロバイダー選定

- トラフィックパターンとグローバルユーザー分布を分析
- CDN プロバイダーを評価 (Cloudflare, AWS CloudFront, Fastly, KeyCDN)
- コンテンツタイプとキャッシュ要件を評価
- エッジロケーション戦略を計画
- パフォーマンスとコスト最適化の目標を定義

## 2. CDN 設定

- Page Rules / Cache Rules でコンテンツタイプ別ルールを設定
- オリジンサーバーとの接続を最適化
- HTTP/2, HTTP/3 を有効化
- SSL/TLS 設定を最適化 (TLS 1.3, OCSP Stapling)

## 3. 静的アセット最適化

バンドルの詳細は `bundle-optimization.md` を参照。

- content-hash 付きファイル名でキャッシュバスティング
- アセットを CDN サブドメインから配信
- 画像最適化パイプラインを構築 (WebP/AVIF 変換、リサイズ)
- フォントのサブセット化と最適配信

## 4. 圧縮最適化

- Gzip (level 6) と Brotli (level 4) を有効化
- ビルド時に事前圧縮 (.gz, .br ファイル生成)
- 圧縮対象: text/html, application/javascript, text/css, application/json
- 画像・動画は圧縮対象外 (既に圧縮済み)

## 5. キャッシュヘッダー・ポリシー

キャッシュ戦略の詳細は `caching.md` を参照。

- **バージョン付きアセット** (JS/CSS/画像): `public, max-age=31536000, immutable`
- **HTML**: `no-cache` (常にオリジンで再検証)
- **API レスポンス**: `private, no-store` または短い max-age
- **フォント**: `public, max-age=31536000`
- ETag / Last-Modified で条件付きリクエストを有効化

## 6. 画像最適化・配信

- レスポンシブ画像: srcset + sizes で適切なサイズを配信
- 複数フォーマット: picture 要素で WebP/AVIF/fallback
- 遅延読み込み: loading="lazy" + Intersection Observer
- CDN の画像変換機能を活用 (Cloudflare Images 等)

## 7. キャッシュパージ・無効化

- デプロイ時の自動パージを設定
- URL/タグベースの選択的パージ
- パージ完了の監視と検証
- ロールバック時のキャッシュ戦略

## 8. パフォーマンス監視

監視の詳細は `monitoring.md` を参照。

- キャッシュヒット率を監視 (目標: > 90%)
- 帯域幅使用量とコストを追跡
- エッジレスポンスタイムを計測
- エラーレート (4xx/5xx) を監視

## 9. セキュリティ設定

- セキュリティヘッダー: HSTS, X-Content-Type-Options, X-Frame-Options
- DDoS 防止とレート制限
- WAF ルールの設定
- ホットリンク防止

## 10. コスト最適化

- ファイルタイプ別の帯域幅使用を分析
- 画像が帯域幅の 60% 超の場合は画像最適化を優先
- 不要なオリジンリクエストを削減
- リクエスト数 vs 帯域幅のバランスを最適化

See also: バンドルサイズは `bundle-optimization.md`、キャッシュは `caching.md` を参照。
