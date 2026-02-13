# Agent Naming Convention

## Suffix Taxonomy (6 種)

| Suffix | 意味 | Agent Loop Phase | AI-DLC Role |
|--------|------|-----------------|-------------|
| `-architect` | 戦略設計・アーキテクチャ判断 | Upstream (Triage~Planning) | Value Orchestrator, AI Strategy Architect |
| `-pro` | 技術/ドメイン専門・実装 | Development (Implementation) | Senior Architect |
| `-reviewer` | 品質ゲート・レビュー・監査 (読み取り中心) | Verification (Auto-Verify~Human Review) | ARE |
| `-operator` | CI/CD・デプロイ・インシデント対応・運用 | Operations (Done~) | Delivery System Operator |
| `-agent` | メタ: オーケストレーション・チケット管理・Skill ライフサイクル | Cross-phase | Agent Orchestration Coach, Prompt Architect |
| (固有名) | WFGY 認知システムのみ | Cross-phase | (Meta tier) |

## 選択フローチャート

1. システム設計や戦略判断が主務？ → `-architect`
2. 特定技術での実装・構築が主務？ → `-pro`
3. 品質レビュー・監査・検証が主務？ → `-reviewer`
4. デプロイ・運用・インシデント対応が主務？ → `-operator`
5. 他エージェントの調整・ツーリング管理が主務？ → `-agent`
6. WFGY 認知コンポーネント？ → 固有名を維持

## WFGY 固有名 (変更なし)

| Agent | 理由 |
|-------|------|
| semantic-architect | WFGY の中心 (※ -architect サフィックスだが WFGY グループ) |
| memory-curator | 固有の認知メタファー |
| boundary-guardian | 固有名 |
| reasoning-validator | 推論検証の特殊用途 |
| cognitive-debugger | 固有の認知メタファー |
| logic-synthesizer | 固有の認知メタファー |
| decision-navigator | 固有の認知メタファー |

## カウント

| Suffix | Count |
|--------|-------|
| `-architect` | 8 |
| `-pro` | 26 |
| `-reviewer` | 7 |
| `-operator` | 7 |
| `-agent` | 12 |
| WFGY 固有名 | 6 (semantic-architect は -architect にもカウント) |
| **合計** | **66** |
