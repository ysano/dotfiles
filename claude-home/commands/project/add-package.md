---
description: "Add and configure new project dependencies"
---

## Instructions

ワークスペースに新しいパッケージを追加・設定する: **$ARGUMENTS**

1. **パッケージ定義**: 名前とタイプ（library/application/tool/service）を特定し、既存パッケージとの名前衝突を確認
2. **構造作成**: 適切なワークスペース位置（packages/, apps/, libs/）にタイプに応じたディレクトリ構造を生成
3. **設定**: package.json（メタデータ、依存関係、scripts、exports）、tsconfig.json（ワークスペース設定を継承）を生成
4. **ワークスペース統合**: ワークスペース設定に登録、クロスパッケージ参照を設定、ビルド順序を構成
5. **テスト・ビルド**: テストフレームワーク設定、初期テスト作成、ビルドシステム構成
6. **検証**: ビルド成功、インポート解決、開発サーバー起動を確認
