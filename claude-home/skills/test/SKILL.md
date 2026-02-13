---
name: test
description: >
  Software testing domain knowledge and procedures.
  Use when writing tests, improving coverage, setting up E2E/load/visual/mutation testing,
  generating test cases, or implementing comprehensive testing strategies.
  Referenced by test-operator and qa-reviewer Agents.
user-invocable: true
---

ソフトウェアテストの設計・実装・自動化・品質保証の手順知識。
Test Pyramid, AAA パターン, カバレッジメトリクスに基づく。

> **Agent 連携**: `test-operator` Agent はテスト自動化戦略の行動原則、
> `qa-reviewer` Agent は品質保証評価の行動原則を持つ。
> 具体的な手順は本 Skill の `references/` を参照して実行する。

## リファレンスガイド

| ドキュメント | 内容 | 参照タイミング |
|---|---|---|
| `references/write-tests.md` | ユニット・統合テスト作成手順 (AAA パターン) | テスト作成・テストフレームワーク利用時 |
| `references/test-coverage.md` | カバレッジ分析・改善手順 (Line/Branch/Function) | カバレッジ向上・ギャップ分析時 |
| `references/e2e-setup.md` | E2E テストセットアップ (Playwright/Cypress) | E2E テスト導入・クロスブラウザテスト時 |
| `references/generate-test-cases.md` | テストケース自動生成手順 | テストケース生成・コード分析時 |
| `references/comprehensive-testing.md` | 包括的テストインフラ構築手順 | テスト戦略策定・全レイヤーテスト導入時 |
| `references/load-testing.md` | 負荷・パフォーマンステストセットアップ | 負荷テスト導入・キャパシティプランニング時 |
| `references/visual-testing.md` | ビジュアルリグレッションテストセットアップ | UI テスト・ビジュアル変更検出時 |
| `references/mutation-testing.md` | ミューテーションテストセットアップ | テスト品質評価・テスト有効性検証時 |
| `references/property-based-testing.md` | プロパティベーステスト実装手順 | 不変条件テスト・ロバストネス検証時 |

## テスト戦略クイックリファレンス

| 戦略 | 目的 | 主要ツール | 関連リファレンス |
|------|------|-----------|-----------------|
| Test Pyramid | ユニット > 統合 > E2E の最適バランス | Jest, Pytest, JUnit | write-tests, comprehensive-testing |
| AAA Pattern | Arrange-Act-Assert による構造化テスト | All frameworks | write-tests |
| Coverage | Line 80%+, Branch 70%+, Function 90%+ | Istanbul, JaCoCo, Coverage.py | test-coverage |
| E2E Testing | ユーザーフロー・クロスブラウザ検証 | Playwright, Cypress | e2e-setup |
| Visual Testing | UI リグレッション検出 | Chromatic, Percy | visual-testing |
| Load Testing | パフォーマンス・容量検証 | k6, Artillery, JMeter | load-testing |
| Mutation Testing | テスト品質・有効性検証 | Stryker, PIT, mutmut | mutation-testing |
| Property-Based | 不変条件・ロバストネス検証 | fast-check, Hypothesis | property-based-testing |

## コマンドとの関係

以下の Commands は本 Skill への薄いエントリーポイント:

| Command | 対応リファレンス |
|---------|-----------------|
| `/test:write-tests` | `references/write-tests.md` |
| `/test:test-coverage` | `references/test-coverage.md` |
| `/test:e2e-setup` | `references/e2e-setup.md` |
| `/test:generate-test-cases` | `references/generate-test-cases.md` |
| `/test:setup-comprehensive-testing` | `references/comprehensive-testing.md` |
| `/test:setup-load-testing` | `references/load-testing.md` |
| `/test:setup-visual-testing` | `references/visual-testing.md` |
| `/test:add-mutation-testing` | `references/mutation-testing.md` |
| `/test:add-property-based-testing` | `references/property-based-testing.md` |

<constraints>
## 行動制約

- **AAA パターン必須**: すべてのテストは Arrange-Act-Assert 構造に従うこと
- **動作テスト優先**: 実装詳細ではなく、観察可能な動作をテストすること
- **テスト分離保証**: テストは独立して実行可能で、互いに干渉しないこと
- **カバレッジと品質両立**: カバレッジ率だけでなく、アサーションの意味性を確保すること
</constraints>
