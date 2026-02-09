---
description: "Configure and initialize a project following the Product as Code specification"
---

## Instructions

Product as Code (PAC) プロジェクトを初期化・設定する: **$ARGUMENTS**

1. **コンテキスト分析**: git リポジトリ確認、既存 PAC 設定（epic-*.yaml, ticket-*.yaml）の有無を検証
2. **インタラクティブセットアップ**（既存設定なしの場合）: プロジェクト名、説明、オーナー、デフォルト担当者、初期エピック名をユーザーに確認
3. **ディレクトリ構造作成**: `.pac/{epics,tickets,templates}/` と README.md
4. **設定ファイル生成**: `.pac/pac.config.yaml`（apiVersion: productascode.org/v0.1.0）
5. **テンプレート作成**: Epic/Ticket テンプレート YAML を `.pac/templates/` に生成
6. **初期データ**: ユーザー入力に基づき最初の Epic と Ticket を作成
7. **バリデーション**: `.pac/scripts/validate.sh` を作成・実行、構造サマリーと次のステップを表示

### 引数

- `--minimal`: テンプレート・スクリプトなしの最小構成
- `--epic-name <name>`: 初期エピック名
- `--owner <name>`: プロダクトオーナー名
- `--no-git`: git 統合をスキップ
