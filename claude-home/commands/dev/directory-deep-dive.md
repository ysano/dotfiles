---
description: "Analyze directory structure and create CLAUDE.md documentation"
---

## Instructions

Analyze the specified directory: **$ARGUMENTS** (default: current working directory)

1. **Structure** - ディレクトリ構成を調査し、各サブディレクトリ/ファイルの役割を特定
2. **Architecture** - 設計パターン、依存関係、命名規約、主要な抽象化を分析
3. **Document** - 分析結果を対象ディレクトリ内の CLAUDE.md に記録（既存の場合は更新）
   - モジュールの目的と責任
   - アーキテクチャ上の決定事項
   - 重要な実装詳細と注意点
