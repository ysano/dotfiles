Organization Topology: AI/人間ハイブリッドチームの規模別構成パターン。

## 3 つのトポロジー

従来の「人数ベース」の規模分類を、AI Agent を含むハイブリッドチーム構成に再定義。

### 1. ケンタウロス・ポッド（小規模 ~5 名）

人間と AI が一体化した最小チーム。1 人の人間が複数の AI Agent を直接操作する。

**構成**:
- アーキテクト: 1 名（技術方針決定 + Spec Definition）
- ARE（AI Review Engineer）: 2-3 名（レビュー + マージ権限）
- AI Agent 群: Claude Code インスタンス（Plan Agent + Build Agent）

**運用**:
- ステータス数: 3-4（Todo / In Progress / Review / Done）
- Agent Loop: 簡略化（Triage 省略、AI Planning + Implementation を一体化）
- トリアージ: 不要。アーキテクトが直接判断
- ツール推奨: **Linear**（シンプル、高速、AI-friendly API）

**チケット管理のポイント**:
- Backlog は 20 件以下に維持
- ラベルは最小限（type のみ）
- スプリントより Kanban フロー推奨

### 2. エージェンティック・スクワッド（中規模 15-50 名）

ドメインごとにポッドを編成し、プラットフォームチームが横断支援する。

**構成**:
- ドメインポッド × 3-5: 各ポッドにアーキテクト 1 名 + ARE 2-4 名 + AI Agent 群
- プラットフォームチーム: CI/CD、共通ライブラリ、AI Agent 基盤の管理
- テックリード: クロスポッド技術調整

**運用**:
- ステータス数: 5-6（Agent Loop の 7 段階から Auto-Verification を CI に統合）
- Agent Loop: フル適用。各ポッドが独立して Agent Loop を回す
- トリアージ: 週 1 回のクロスポッド同期。ポッド内は日次
- ツール推奨: **GitHub Projects**（コードとの密結合、PR 自動リンク）

**チケット管理のポイント**:
- Epic でポッド間の依存関係を管理
- ラベル: type + priority + component（ポッド名）
- スプリント: 1-2 週間サイクル
- カスタムフィールド: AI-Confidence、Turns-Used を追加

### 3. エンタープライズ・プラットフォーム（大規模 100 名超）

AI CCoE（Cloud Center of Excellence）がガバナンスを統括する。

**構成**:
- AI CCoE: AI Agent のガバナンス、プロンプトライブラリ管理、品質基準策定
- ドメインクラスター × 5-10: 各クラスター内に複数のポッド
- SRE / Platform Engineering: AI Agent 実行基盤の運用
- セキュリティチーム: AI 生成コードのセキュリティレビュー

**運用**:
- ステータス数: 7（Agent Loop フル適用）
- Agent Loop: フル適用 + クラスター間調整レイヤー
- トリアージ: 週 2 回 + 自動化ルールで検出
- ツール推奨: **Jira + Linear ハイブリッド**（Jira で全社ガバナンス、Linear でポッド内開発）

**チケット管理のポイント**:
- 階層: Epic → Story/Task → Sub-task の 3 階層
- カスタムフィールド: AI-Confidence + Turns-Used + Review-Priority + Security-Flag
- 自動化: Stale 検出、Agent Drift 検知、レポート自動生成
- コンプライアンス: AI 生成コード比率の追跡、監査ログ

## Agent Drift 防止策（中〜大規模向け）

AI Agent が意図しない方向に進む「Agent Drift」を防止する仕組み:

| 防止策 | 内容 | 適用規模 |
|---|---|---|
| **Plan 承認必須** | AI Planning の出力を人間が承認してから Implementation | 全規模 |
| **Churn 監視** | 往復 3 回超で自動アラート | 中規模〜 |
| **Diff サイズ制限** | PR 差分 500 行超で自動警告 | 中規模〜 |
| **プロンプト監査** | AI Agent への入力プロンプトをログ記録・定期レビュー | 大規模 |
| **モデルバージョン固定** | Agent が使用する LLM バージョンを環境ごとに固定 | 大規模 |
| **Guardrails** | 禁止パターン（rm -rf、force push 等）の自動ブロック | 全規模 |

## トポロジー選択基準

| 基準 | ケンタウロス・ポッド | エージェンティック・スクワッド | エンタープライズ |
|---|---|---|---|
| **チーム人数** | ~5 名 | 15-50 名 | 100 名超 |
| **プロダクト数** | 1 | 2-5 | 5+ |
| **AI Agent 成熟度** | 実験〜初期導入 | 本格運用 | 全社標準 |
| **ガバナンス要件** | なし | チーム内ルール | 全社ポリシー必須 |
| **推奨ステータス数** | 3-4 | 5-6 | 7 |
| **推奨ツール** | Linear | GitHub Projects | Jira + Linear |

**移行のサイン**:
- ポッド → スクワッド: ポッド間の依存関係が頻発（月 5 件超）
- スクワッド → エンタープライズ: セキュリティ・コンプライアンス要件の増大
