---
name: security
description: >
  Application security domain knowledge and procedures.
  Use when performing security audits, hardening applications, auditing dependencies,
  implementing authentication systems, or reviewing OWASP Top 10 compliance.
  Referenced by security-reviewer Agent.
user-invocable: true
---

アプリケーションセキュリティの監査・堅牢化・依存関係管理・認証実装の手順知識。
OWASP Top 10 / NIST / CIS Benchmarks に基づく。

> **Agent 連携**: `security-reviewer` Agent はセキュリティ評価の行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/security-audit.md` | 10 ステップ包括的セキュリティ監査手順 | セキュリティ評価・脆弱性発見時 |
| `references/security-hardening.md` | 10 ステップ堅牢化実装手順 | 脆弱性修正・セキュリティ強化時 |
| `references/dependency-audit.md` | 13 ステップ依存関係監査手順 | 依存関係の脆弱性・ライセンス確認時 |
| `references/authentication.md` | 10 ステップ認証システム実装ガイド | 認証・認可機能の設計・実装時 |

## OWASP Top 10 クイックリファレンス

| # | カテゴリ | 主な対策 | 関連リファレンス |
|---|---------|---------|-----------------|
| A01 | Broken Access Control | RBAC、最小権限、CORS 制限 | audit, hardening, authentication |
| A02 | Cryptographic Failures | TLS 強制、適切な暗号化、鍵管理 | hardening |
| A03 | Injection | パラメータ化クエリ、入力検証、出力エンコード | audit, hardening |
| A04 | Insecure Design | 脅威モデリング、セキュアデザインパターン | audit |
| A05 | Security Misconfiguration | セキュリティヘッダー、デフォルト設定変更 | hardening |
| A06 | Vulnerable Components | 依存関係スキャン、SCA ツール | dependency-audit |
| A07 | Auth Failures | MFA、セッション管理、ブルートフォース防止 | authentication |
| A08 | Data Integrity Failures | CI/CD セキュリティ、署名検証 | dependency-audit, hardening |
| A09 | Logging Failures | セキュリティイベントログ、監視 | audit, hardening |
| A10 | SSRF | URL 検証、ネットワーク分離 | audit |

## コマンドとの関係

以下の Commands は本 Skill への薄いエントリーポイント:

| Command | 対応リファレンス |
|---------|-----------------|
| `/security:security-audit` | `references/security-audit.md` |
| `/security:security-hardening` | `references/security-hardening.md` |
| `/security:dependency-audit` | `references/dependency-audit.md` |
| `/security:add-authentication-system` | `references/authentication.md` |

<constraints>
## 行動制約

- **重大度分類必須**: 発見事項は必ず重大度レベル (Critical / High / Medium / Low) で分類すること
- **手動レビュー補完**: 自動スキャンツールの結果だけで完了とせず、手動レビューで補完すること
- **シークレット即時報告**: ハードコードされたシークレット・API キー・パスワードを検出した場合、ユーザーに即座に報告すること
- **修正手順付き**: 脆弱性の指摘には必ず具体的な修正手順またはコード例を含めること
</constraints>
