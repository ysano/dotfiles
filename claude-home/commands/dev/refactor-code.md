---
description: "Refactor code to improve structure, readability, or performance"
---

## 実行手順

Refactor the following code: **$ARGUMENTS**

1. **Analysis** - 対象コードの現状を理解し、リファクタリング目標を明確化（可読性/保守性/パフォーマンス）
2. **Test Baseline** - 既存テストカバレッジを確認。不足していれば先にテストを追加
3. **Branch** - `git checkout -b refactor/[scope]`
4. **Incremental Changes** - 小さな変更単位で実施、各ステップでテスト実行。頻繁にコミット
5. **Quality Check** - Linter/静的解析を実行、パフォーマンスが劣化していないことを確認
