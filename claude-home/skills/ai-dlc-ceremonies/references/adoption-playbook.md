# AI-DLC Adoption Playbook

AI-DLC を段階的に導入するための実践ガイド。
ビッグバン導入ではなく、既存の `/team:*` コマンドから `/ai-dlc:*` へ段階的に移行する。

## Why: 段階的導入が必要な理由

- AI-DLC は役割・プロセス・メトリクスを同時に変える変革であり、一度にすべてを変えるとチームが混乱する
- 既存のスクラムイベント（Planning / Standup / Retro）を完全に置き換えるのではなく、AI ネイティブな要素を**段階的に追加**する
- パイロットチームでの検証→定着→スケールアップの順序で進める

---

## 5-Step Adoption

### Step 1: 現状分析（1-2日）

**目的**: 現在の状態を棚卸しし、AI-DLC 導入の出発点を確認する。

**アクション**:
- `/ai-dlc:calibrate` を実行し、CLAUDE.md / SKILL / Hooks の現状を分析
- 各役割の時間配分を可視化（AI 自動化可能な業務の割合を特定）
- 既存の `/team:*` コマンド利用状況を確認

**完了基準**:
- CLAUDE.md が存在し、プロジェクトコンテキストが記述されている
- Hooks 基本設定（`bash-validator.py` 等）が有効化されている
- 各役割の現状業務配分が可視化されている

### Step 2: パイロットチーム（2-4週間）

**目的**: ケンタウロス・ポッド（1-10名）で AI-DLC セレモニーを試行する。

**アクション**:
- `/ai-dlc:plan` で初回スプリント計画を実施
  - 従来の `/team:sprint-planning` との違い: AI 事前分析 + Spec 品質チェックが追加
- `/ai-dlc:digest` を毎日実行
  - 従来の `/team:standup-report` との違い: 4h ルール違反検知 + Churn アラートが追加
- Atomic Spec テンプレートの導入を開始

**成功基準**:
- MTTV（平均検証時間）のベースラインを測定
- Interaction Churn の初期値を記録
- Outcome Done 率（Output Done のうち実際に価値を届けたもの）を測定開始

### Step 3: セレモニー定着（4-8週間）

**目的**: AI-DLC セレモニーを定常運用に組み込む。

**アクション**:
- `/ai-dlc:diagnose` でレトロスペクティブを実施し改善サイクルを確認
  - 従来の `/team:retrospective-analyzer` との違い: Churn 根因分析 + CLAUDE.md 改善提案が追加
- `/ai-dlc:verify` でレビュー品質を定量評価
- Hooks プリセット `settings-ai-dlc.json` を導入
  - `check-spec-existence.py`: コード変更前の Spec 存在確認
  - `auto-update-ticket.sh`: git push 後のチケット自動更新

**完了基準**:
- 3スプリント以上の継続運用
- Diagnostic Session のデータが蓄積されている
- Hooks がチームのワークフローに定着している

### Step 4: 役割移行（継続的）

**目的**: チームメンバーの役割認識を AI-DLC に合わせて更新する。

**アクション**:
- `references/role-transformation.md` の内容をチームで共有・議論
- ワーキングアグリーメントを更新（AI-人間の責務分担を明文化）
  - AI は作業を生成、人間がアカウンタビリティを持つ
  - AI エージェントの行動範囲と停止条件
- メトリクスを刷新:
  - MTTV（平均検証時間）
  - Interaction Churn（AI との手戻り回数）
  - Outcome Done 達成率

**注意点**:
- 役割の「名前」を変えることが目的ではない。焦点の変化を認識することが重要
- 心理的安全性を確保し、「自分の仕事がなくなる」不安に対処する

### Step 5: スケールアップ（チーム判断）

**目的**: Pod → Squad → Enterprise へ展開する。

**アクション**:
- `/ai-dlc:calibrate` の隔週定期実行で設定ドリフトを検知
- `ticket-management` Skill との統合深化
  - Atomic Spec → Agent Loop → DoD の一貫したフロー
- 複数チーム間のベストプラクティス共有
  - CLAUDE.md / Hooks 設定のテンプレート化
  - チーム横断のメトリクスダッシュボード

**スケール別の考慮事項**:

| スケール | 追加で必要なもの |
|---|---|
| **Pod (1-10名)** | 基本の 5 コマンドで十分 |
| **Squad (10-30名)** | チーム間の Spec 品質基準統一、共有 Hooks 設定 |
| **Enterprise (30名以上)** | AI CCoE との連携、コンプライアンス対応、コスト管理 |

---

## 段階別チェックリスト

| 段階 | 完了基準 | 検証コマンド |
|---|---|---|
| **導入前** | CLAUDE.md 存在、Hooks 基本設定 | `/ai-dlc:calibrate` |
| **パイロット中** | 3スプリント継続、メトリクス測定開始 | `/ai-dlc:diagnose` |
| **定着後** | Churn ≤ 3、MTTV ≤ 1h | `/ai-dlc:verify` |
| **スケール中** | 複数チーム展開、設定テンプレート化 | `/ai-dlc:calibrate`（隔週） |

## コマンド移行マップ

既存の `/team:*` コマンドとの対応関係:

| 従来 | AI-DLC | 追加される機能 |
|---|---|---|
| `/team:sprint-planning` | `/ai-dlc:plan` | AI 事前分析、Spec 品質チェック |
| `/team:standup-report` | `/ai-dlc:digest` | 4h ルール違反検知、Churn アラート |
| `/team:retrospective-analyzer` | `/ai-dlc:diagnose` | Churn 根因分析、CLAUDE.md 改善提案 |
| （なし） | `/ai-dlc:verify` | AI 出力検証、Outcome Done ギャップ分析 |
| （なし） | `/ai-dlc:calibrate` | CLAUDE.md/SKILL/Hooks 定期検査 |

> `/team:*` コマンドは汎用チーム運営ツールとして残る。`/ai-dlc:*` は AI-DLC フレームワークに特化した拡張版。
