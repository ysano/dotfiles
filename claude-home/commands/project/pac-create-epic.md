---
description: "Create a new epic following the Product as Code specification"
---

## Instructions

PAC 仕様に従って新しい Epic を作成する: **$ARGUMENTS**

1. **環境検証**: `.pac/` ディレクトリと `pac.config.yaml` の存在を確認（なければ `/project:pac-configure` を案内）
2. **情報収集**: 引数または対話的に Epic の名前、説明、オーナー、スコープ、成功基準を取得
3. **ID 生成**: エピック名から `epic-[kebab-case]` 形式で生成、既存 Epic との一意性を検証
4. **YAML 作成**: PAC v0.1.0 仕様に準拠した Epic YAML を `.pac/epics/[epic-id].yaml` に書き出し
5. **検証・保存**: 必須フィールド・YAML 構文を検証、成功時にサマリーと次のステップ（チケット作成コマンド等）を表示

### 引数

- `--name <name>`: Epic 名
- `--description <desc>`: 説明
- `--owner <owner>`: オーナー
- `--priority <low|medium|high|critical>`: 優先度
- `--no-git`: git 統合をスキップ
