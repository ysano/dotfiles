---
description: "Validate Product as Code project structure and files for specification compliance"
---

## Instructions

PAC プロジェクトの構造とファイルを仕様準拠で検証する: **$ARGUMENTS**

1. **環境確認**: `.pac/` ディレクトリと `pac.config.yaml` の存在・妥当性を検証
2. **ディレクトリ構造**: 必須ディレクトリ（epics/, tickets/）の存在、孤立ファイルの検出
3. **Epic 検証**: 各 Epic ファイルの YAML 構文、apiVersion、kind、必須メタデータ（id, name, created, owner）、成功基準の存在
4. **Ticket 検証**: 各 Ticket ファイルの YAML 構文、必須メタデータ、type/status/priority の有効値、受け入れ基準の存在
5. **相互参照**: Ticket→Epic 参照の整合性、Epic チケットリストと実ファイルの一致、循環依存の検出、全 ID の一意性
6. **データ整合性**: タイムスタンプ形式（ISO 8601）、updated >= created の検証
7. **レポート生成**: 検証結果サマリー（有効/無効件数、問題詳細、修正推奨）を出力

### 引数

- `--file <path>`: 特定ファイルのみ検証
- `--epic <epic-id>`: 特定 Epic とその Ticket のみ検証
- `--fix`: 一般的な問題を自動修正（バックアップ作成後）
- `--pre-commit`: pre-commit モード（ステージ済みファイルのみ、簡潔出力）
- `--verbose`: 詳細情報を表示
