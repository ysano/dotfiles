# AI-DLC Lifecycle Simulation

3 ペルソナ × 6 フェーズのマトリクスで、310 モジュールの活用状況・ギャップ・重複を分析する。

## ペルソナ定義

| ペルソナ | 規模 | 特徴 |
|----------|------|------|
| **Solo** | 1人 + AI Fleet | `/ai-dlc:quick-spec` 中心、セレモニー軽量、GitHub Issues で十分 |
| **Pod** | 3-5人 + AI Fleet | フルパイプライン、1-3日スプリント、GitHub Projects V2 |
| **Squad** | 10-30人 | マルチ Pod、Linear/Jira 連携、ガバナンス重要、1週間スプリント |

---

## Phase 1: Upstream (要件定義・設計)

### Solo

**使用モジュール**: `/ai-dlc:quick-spec`, `/spec-elicitation`, `ai-dlc-upstream` skill
**体験フロー**:
1. `/spec-elicitation` でインタビュー形式で要件整理
2. `/ai-dlc:quick-spec` で単一文書 Spec を生成
3. そのまま実装へ（PRD/Architecture フェーズをスキップ）

**ギャップ**: なし。軽量パスが適切に機能する。

### Pod

**使用モジュール**: `/ai-dlc:create-prd`, `/ai-dlc:create-architecture`, `/ai-dlc:create-stories`, `/ai-dlc:translate-upstream`, `ai-dlc-upstream` skill, `ai-dlc-estimate` skill, `product-architect` agent
**体験フロー**:
1. `/ai-dlc:create-prd` でインタビュー駆動 PRD 作成
2. `/ai-dlc:create-architecture` で ADR 駆動設計
3. `/ai-dlc:create-stories` で Atomic Spec スタブ群生成
4. `product-architect` agent で戦略レビュー

**ギャップ**: 上流品質ゲート (Gate 1-4) がフック自動化されていない (G7)。手動確認で対応可能。

### Squad

**使用モジュール**: Pod の全モジュール + `strategy-architect` agent, `ai-dlc-sier` skill, `simulation` skill
**体験フロー**:
1. Pod と同じパイプラインを各 Pod で実行
2. `strategy-architect` で cross-Pod 依存関係分析
3. `/ai-dlc:translate-upstream` で外部文書（BMAD 等）を統一フォーマットに変換

**ギャップ**: 同上 (G7)。Squad 規模では手動ゲート確認のコストが増大。

---

## Phase 2: Planning (スプリント計画)

### Solo

**使用モジュール**: `/ai-dlc:plan` (軽量モード), `ticket-management` skill
**体験フロー**:
1. `/ai-dlc:plan` で Phase 1 (AI Pre-Planning) のみ実行
2. Phase 2-3 は自分で判断（1人なので戦略ブリーフィング不要）
3. GitHub Issues にラベル付けして開始

**ギャップ**: バックログリファインメント (G2) がない。Solo は影響小。

### Pod

**使用モジュール**: `/ai-dlc:plan`, `/ai-dlc:refine` (**新規**), `/ai-dlc:digest`, `ai-dlc-ceremonies` skill, `ticket-management` skill, `orchestration-agent` agent
**体験フロー**:
1. `/ai-dlc:refine` でバックログ精査（Sprint 計画の前段）
2. `/ai-dlc:plan` で 3 フェーズ計画実行
3. `/ai-dlc:digest` でデイリー非同期ダイジェスト
4. `orchestration-agent` で複合タスク分解

**ギャップ**: ライフサイクル健全性ダッシュボード (G1) がない。Sprint 中の全体俯瞰が手動。

### Squad

**使用モジュール**: Pod の全モジュール + `github-projects-v2`/`linear`/`jira` skill, `team-agent` agent, `github-board-agent` agent
**体験フロー**:
1. 各 Pod が `/ai-dlc:refine` → `/ai-dlc:plan` を実行
2. `team-agent` で cross-Pod 調整
3. `github-board-agent` で Project ボード更新

**ギャップ**: G1 (ダッシュボード) の影響が最大。複数 Pod の健全性を一元管理する手段がない。

---

## Phase 3: Development (Agent Loop: 実装)

### Solo

**使用モジュール**: `/dev:incremental-feature-build`, `/dev:debug-error`, `/dev:code-review`, `setup` skill, 各言語 agent (typescript-pro, python-pro 等)
**体験フロー**:
1. Agent Loop: Triage → Spec Definition → AI Planning → AI Coding
2. `/dev:incremental-feature-build` で段階的実装
3. AI 生成コードを自分でレビュー

**ギャップ**: なし（`churn-counter.py` フック実装済み）。

### Pod

**使用モジュール**: Solo の全モジュール + `/dev:parallel-feature-build`, `/dev:pull-request`, `docs` skill, `fullstack-pro` agent, `debug-pro` agent
**体験フロー**:
1. 複数メンバーが並列で Agent Loop 実行
2. `/dev:parallel-feature-build` で並列エージェント構築
3. `/dev:pull-request` で PR 作成

**ギャップ**: なし（`churn-counter.py` フック実装済み）。

### Squad

**使用モジュール**: Pod の全モジュール + `graphql-architect`, `backend-architect` agent 等
**体験フロー**:
1. 各 Pod が独立して Development 実行
2. cross-Pod PR は `/ai-dlc:review` で品質ゲート

**ギャップ**: `orchestration/*` と AI-DLC Agent Loop が未接続 (G6)。大規模オーケストレーションに手動調整が必要。

---

## Phase 4: Verification (検証)

### Solo

**使用モジュール**: `/ai-dlc:review`, `/test:write-tests`, `check-spec-existence.py` hook, `test` skill, `security` skill
**体験フロー**:
1. `check-spec-existence.py` hook で Spec 存在確認（自動）
2. `/test:write-tests` でテスト作成
3. `/ai-dlc:review` でセルフレビュー

**ギャップ**: なし（`check-spec-existence.py` に Atomic Spec 5要素品質スコアリング実装済み）。

### Pod

**使用モジュール**: Solo の全モジュール + `/ai-dlc:verify`, `/security:security-audit`, `/performance:performance-audit`, `code-reviewer` agent, `security-reviewer` agent, `qa-reviewer` agent
**体験フロー**:
1. PR ごとに `/ai-dlc:review` で品質ゲート
2. Sprint 終了時に `/ai-dlc:verify` で成果検証
3. `code-reviewer`, `security-reviewer` で多角的レビュー

**ギャップ**: なし（`metrics-collector.py` フック実装済み）。

### Squad

**使用モジュール**: Pod の全モジュール + `architecture-reviewer` agent, `performance-reviewer` agent, `test-operator` agent
**体験フロー**:
1. Pod 単位の Verification + cross-Pod 統合テスト
2. `architecture-reviewer` でアーキテクチャ整合性確認
3. `test-operator` で CI/CD パイプライン統合

**ギャップ**: なし（G4, G5 フック実装済み）。

---

## Phase 5: Operations (運用・デプロイ)

### Solo

**使用モジュール**: `/deploy:prepare-release`, `/deploy:ci-setup`, `auto-update-ticket.sh` hook, `deploy` skill
**体験フロー**:
1. `/deploy:prepare-release` でリリース準備
2. `auto-update-ticket.sh` で push 時に Issue 自動更新
3. シンプルなデプロイフロー

**ギャップ**: なし。Solo には十分なツールセット。

### Pod

**使用モジュール**: Solo の全モジュール + `/deploy:containerize-application`, `/deploy:hotfix-deploy`, `/deploy:rollback-deploy`, `deploy-operator` agent, `release-operator` agent
**体験フロー**:
1. コンテナ化 + CI/CD パイプライン構築
2. `deploy-operator` でパイプライン管理
3. ホットフィックス / ロールバック対応

**ギャップ**: なし。

### Squad

**使用モジュール**: Pod の全モジュール + `/deploy:setup-kubernetes-deployment`, `cloud-architect` agent, `azure-devops-operator` agent, `incident-operator` agent, `cloudflare-manager` skill
**体験フロー**:
1. K8s デプロイ + クラウドインフラ管理
2. `incident-operator` でインシデント対応
3. `cloudflare-manager` でエッジ配信

**ギャップ**: なし。Operations は最も充実したフェーズ。

---

## Phase 6: Improvement (振り返り・改善)

### Solo

**使用モジュール**: `/ai-dlc:diagnose`, `/ai-dlc:calibrate`, `session-learning-capture.sh` hook
**体験フロー**:
1. `/ai-dlc:diagnose` で Before/During/After 分析
2. `/ai-dlc:calibrate` で CLAUDE.md/SKILL 調整
3. `session-learning-capture.sh` で学習記録

**ギャップ**: なし（`metrics-collector.py` フック実装済み）。

### Pod

**使用モジュール**: Solo の全モジュール + `/ai-dlc:status` (**新規**), `legacy-pro` agent, `dx-pro` agent
**体験フロー**:
1. `/ai-dlc:status` でライフサイクル健全性確認
2. `/ai-dlc:diagnose` でデータ駆動レトロ
3. `/ai-dlc:calibrate` でエージェント調整
4. `dx-pro` で開発体験改善

**ギャップ**: なし（`metrics-collector.py` フック実装済み）。

### Squad

**使用モジュール**: Pod の全モジュール
**体験フロー**:
1. 各 Pod が `/ai-dlc:diagnose` 実行
2. cross-Pod の改善パターンを集約
3. `/ai-dlc:calibrate` で組織全体のエージェント調整

**ギャップ**: なし（`metrics-collector.py` フック実装済み）。

---

## 発見事項サマリ

### A. ギャップ（不足）

| ID | フェーズ | 不足内容 | 影響度 | 対応 |
|----|----------|----------|--------|------|
| G1 | 全体 | ライフサイクル健全性ダッシュボード（`/ai-dlc:status`）がない | 高 | **今回実装** |
| G2 | Planning | バックログリファインメント（`/ai-dlc:refine`）がない。plan は Sprint 計画で、Backlog 精査セレモニーが欠落 | 高 | **今回実装** |
| G3 | Development | Churn カウンター（3/7 回ルール）のフック自動化がない | 中 | **実装済み** (`churn-counter.py`) |
| G4 | Verification | `check-spec-existence.py` が存在確認のみで品質スコアリングしない | 中 | **実装済み** (Atomic Spec 5要素スコアリング追加) |
| G5 | Improvement | AI メトリクス（AI-Confidence, MTTV, Turns-Used）の自動収集がない | 中 | **実装済み** (`metrics-collector.py`) |
| G6 | 全体 | `orchestration/*` と AI-DLC Agent Loop が未接続 | 中 | 将来課題 |
| G7 | Upstream | 上流品質ゲート (Gate 1-4) がフック自動化されていない | 低 | 将来課題 |
| G8 | 全体 | Hook イベント利用率 36%（14 中 5 のみ使用） | 低 | 将来課題 |

### B. 重複（過剰） → 一本化

| ID | 削除対象 | 統合先 | 理由 |
|----|----------|--------|------|
| O1 | `team/sprint-planning` | `/ai-dlc:plan` | ai-dlc 版が完全上位互換（Spec 品質チェック + 3 Phase + AI メトリクス） |
| O2 | `team/standup-report` | `/ai-dlc:digest` | ai-dlc 版が完全上位互換（4h ルール + Churn 検知 + 2 層構造） |
| O3 | `team/retrospective-analyzer` | `/ai-dlc:diagnose` | ai-dlc 版が完全上位互換（Churn 根因分析 + CLAUDE.md 改善提案 + Before/During/After） |

### C. チューニング

| ID | 対象 | 改善内容 |
|----|------|----------|
| T1 | `commands/ai-dlc/README.md` | 新コマンド追加 + セレモニー頻度表追加 |
| T2 | `commands/team/README.md` | 削除 3 コマンドを除去、Related セクション更新 |
| T3 | `docs/catalog.md` | Phase Mapping フロー矢印追加、Commands 数更新、削除反映 |

---

## 将来課題詳細

### G3: Churn カウンター自動化 -- 実装済み

`hooks/churn-counter.py` (PostToolUse: Write/Edit/MultiEdit) として実装。
同一ファイルの修正回数を自動追跡し、3 回で警告、7 回で Spec Definition 差し戻し推奨。

### G4: Spec 品質スコアリング -- 実装済み

`check-spec-existence.py` を拡張。Atomic Spec 5 要素（Context, Current Behavior, Expected Behavior, Constraints, Verification）を正規表現でスコアリングし、3/5 未満で不足要素を警告。mtime ベースのキャッシュ付き。

### G5: AI メトリクス自動収集 -- 実装済み

`hooks/metrics-collector.py` (Stop event) として実装。
セッション JSONL から Turns-Used、ツール使用分布、変更ファイルを自動収集し `~/.claude/metrics/sessions.jsonl` に記録。

### G6: orchestration と Agent Loop の接続

**現状**: `/orchestration:*` はタスクフォルダベースの管理。AI-DLC Agent Loop は Atomic Spec ベース。両者は独立動作。
**理想**: `orchestration:start` が Atomic Spec をタスクソースとして認識し、Agent Loop ステージ遷移を反映。
**実装案**: `orchestration:start` に `--ai-dlc` フラグ追加、または `ai-dlc:dispatch` コマンド新設。

### G7: 上流品質ゲートのフック自動化

**現状**: ai-dlc-upstream Skill で Gate 1-4 を定義（PRD 完了 → Architecture 完了 → Stories 完了 → Sprint Ready）。手動確認。
**理想**: PreToolUse フックで create-architecture 実行前に PRD 存在確認、create-stories 実行前に Architecture 確認。
**実装案**: `hooks/upstream-gate-check.py` (PreToolUse: Skill)

### G8: Hook イベント利用率

**現状**: 利用中イベント: PreToolUse, PostToolUse, Stop, UserPromptSubmit, Notification (5/14)。未使用: SubagentToolUse 系等。
**改善方向**: SubagentToolUse を活用して、サブエージェントの出力品質を自動チェック。ただし現時点ではユースケースが限定的。
