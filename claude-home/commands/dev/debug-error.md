---
description: "Diagnose and fix runtime errors using systematic root cause analysis"
---

## 実行手順

Diagnose and resolve: **$ARGUMENTS**

1. **Error Collection** - エラーメッセージ、スタックトレース、再現条件を収集
2. **Reproduce** - 最小再現ケースを作成し、一貫して発生することを確認
3. **Root Cause Analysis** - スタックトレースを追跡し、原因コードを特定。仮説を立てて検証
4. **Fix Implementation** - 根本原因に対処する最小変更を実装
5. **Verification** - 修正がエラーを解消し、既存テストが通ることを確認。再発防止テストを追加
