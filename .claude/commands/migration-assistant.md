# 移行アシスタント

GitHub環境間でのプロジェクト移行、リポジトリ統合、チーム再編成を支援する包括的な移行ツール。Issues、Projects、Discussions、リポジトリ設定の安全で完全な移行を保証します。

## Instructions

1. **前提条件の確認**
   - GitHub CLI（`gh`）がインストールされ認証済みであることを確認
   - GitHub API接続とトークン権限を確認
   - ソースとターゲットリポジトリの管理権限を確認
   - バックアップストレージが利用可能であることを確認

2. **移行パラメータの解析**
   - 以下からアクションとオプションを抽出: **$ARGUMENTS**
   - 有効なアクション: plan, analyze, migrate, verify, rollback
   - ソースとターゲットリポジトリを決定
   - 移行範囲とフィルタを設定

3. **移行環境の初期化**
   - 移行ワークスペースディレクトリを作成
   - ログ記録と監査証跡を設定
   - チェックポイントシステムを初期化
   - ロールバック機構を準備

4. **Execute Migration Action**
   Based on the selected action:

   ### Plan Action
   - Analyze source system structure
   - Map fields between systems
   - Identify potential conflicts
   - Generate migration strategy
   - Estimate time and resources
   - Create detailed migration plan

   ### Analyze Action
   - Count items to migrate
   - Check data compatibility
   - Identify custom fields
   - Assess attachment sizes
   - Calculate migration impact
   - Generate pre-migration report

   ### Migrate Action
   - Create full backup of source data
   - Execute migration in batches
   - Transform data between formats
   - Preserve relationships
   - Handle attachments and media
   - Create progress checkpoints
   - Log all operations

   ### Verify Action
   - Compare source and target data
   - Validate all items migrated
   - Check relationship integrity
   - Verify custom field mappings
   - Test cross-references
   - Generate verification report

   ### Rollback Action
   - Load rollback checkpoint
   - Restore original state
   - Clean up partial migrations
   - Verify rollback completion
   - Generate rollback report

## Usage
```bash
migration-assistant [action] [options]
```

## アクション
- `plan` - 移行プランの作成
- `analyze` - 移行範囲の評価
- `migrate` - 移行の実行
- `verify` - 移行結果の検証
- `rollback` - 移行のロールバック

## オプション
- `--source <repo>` - ソースリポジトリ（owner/repo）
- `--target <repo>` - ターゲットリポジトリ（owner/repo）
- `--scope <items>` - 移行するアイテム（all/issues/prs/projects/discussions/releases）
- `--dry-run` - 移行のシミュレーション
- `--parallel <n>` - 並列処理スレッド数
- `--checkpoint` - チェックポイント回復を有効化
- `--mapping-file <path>` - カスタムフィールドマッピング
- `--preserve-ids` - 参照IDの維持
- `--archive-source` - 移行後のアーカイブ

## Examples
```bash
# Plan repository migration
migration-assistant plan --source owner/old-repo --target owner/new-repo

# Analyze migration scope
migration-assistant analyze --scope all --source owner/repo

# Dry run migration
migration-assistant migrate --dry-run --parallel 4

# Execute migration with checkpoints
migration-assistant migrate --checkpoint --backup

# Verify migration completeness
migration-assistant verify --deep-check

# Rollback if needed
migration-assistant rollback --transaction-id 12345
```

## Migration Phases

### 1. Planning Phase
- Inventory source data
- Map data structures
- Identify incompatibilities
- Estimate migration time
- Generate migration plan

### 2. Preparation Phase
- Create full backup
- Validate permissions
- Set up target structure
- Configure mappings
- Test connectivity

### 3. Migration Phase
- Transfer data in batches
- Maintain relationships
- Preserve metadata
- Handle attachments
- Update references

### 4. Verification Phase
- Compare record counts
- Validate data integrity
- Check relationships
- Verify attachments
- Test functionality

### 5. Finalization Phase
- Update documentation
- Redirect webhooks
- Archive source data
- Generate reports
- Train users

## Data Mapping Configuration
```yaml
mappings:
  github_to_github:
    issue:
      title: title
      body: body
      state: state
      labels: labels
      milestone: milestone
      assignees: assignees
    
    project:
      name: name
      description: description
      columns: columns
      cards: items
      
    custom_fields:
      - source: "project.status"
        target: "project_v2.status"
        transform: "map_status"
      
    relationships:
      - type: "issue-project"
        source: "project_card"
        target: "project_item"
```

## Migration Safety Features

### Pre-Migration Checks
- Storage capacity verification
- API rate limit assessment
- Permission validation
- Dependency checking
- Conflict detection

### During Migration
- Transaction logging
- Progress tracking
- Error recovery
- Checkpoint creation
- Performance monitoring

### Post-Migration
- Data verification
- Integrity checking
- Performance testing
- User acceptance
- Rollback readiness

## Checkpoint Recovery
```json
{
  "checkpoint": {
    "id": "mig-20240120-1430",
    "progress": {
      "total_items": 5000,
      "completed": 3750,
      "failed": 12,
      "pending": 1238
    },
    "state": {
      "last_processed_id": "issue-3750",
      "batch_number": 75,
      "error_count": 12
    }
  }
}
```

## Rollback Capabilities
- Point-in-time recovery
- Selective rollback
- Relationship preservation
- Audit trail maintenance
- Zero data loss guarantee

## Performance Optimization
- Batch processing
- Parallel transfers
- API call optimization
- Caching strategies
- Resource monitoring

## Migration Reports
- Executive summary
- Detailed item mapping
- Error analysis
- Performance metrics
- Recommendation list

## Common Migration Scenarios

### Repository Consolidation
1. Merge multiple repositories into one
2. Combine issues and preserve references
3. Merge project boards and workflows
4. Consolidate documentation and wikis
5. Unify team permissions and settings

### Repository Splitting
1. Extract specific components to new repos
2. Filter issues by labels or components
3. Split project boards by feature areas
4. Preserve commit history for relevant files
5. Update cross-references and links

### Organization Migration
1. Move repositories between organizations
2. Transfer team memberships and permissions
3. Migrate organization-level projects
4. Update webhooks and integrations
5. Preserve collaboration history

## Required Dependencies
- GitHub CLI (gh) with proper authentication
- Git with repository access
- GitHub API token with appropriate scopes

## Error Handling
- Automatic retry with backoff
- Detailed error logging
- Partial failure recovery
- Manual intervention points
- Comprehensive error reports

## Best Practices
- Always run analysis first
- Use dry-run for testing
- Migrate in phases for large datasets
- Maintain communication with team
- Keep source data until verified
- Document custom mappings
- Test rollback procedures

## Compliance & Audit
- Full audit trail
- Data retention compliance
- Privacy preservation
- Change authorization
- Migration certification

## Notes
This command creates a complete GitHub migration package including backups, logs, and documentation. The migration can be resumed from checkpoints in case of interruption. All migrations are reversible within the retention period using GitHub's built-in features.