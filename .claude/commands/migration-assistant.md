# 移行アシスタント

GitHub IssuesとLinearの間での包括的なデータ保持、検証、ロールバック機能を備えたチーム移行を支援。このエンタープライズ対応コマンドは安全で完全な移行を保証します。

## Instructions

1. **前提条件の確認**
   - GitHub CLI（`gh`）がインストールされ認証済みであることを確認
   - Linear MCPサーバーが接続されているかチェック
   - 両システムで十分な権限があることを確認
   - バックアップストレージが利用可能であることを確認

2. **移行パラメータの解析**
   - 以下からアクションとオプションを抽出: **$ARGUMENTS**
   - 有効なアクション: plan, analyze, migrate, verify, rollback
   - ソースとターゲットシステムを決定
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
- `--source <system>` - ソースシステム（github/linear）
- `--target <system>` - ターゲットシステム（github/linear）
- `--scope <items>` - 移行するアイテム（all/issues/prs/projects）
- `--dry-run` - 移行のシミュレーション
- `--parallel <n>` - 並列処理スレッド数
- `--checkpoint` - チェックポイント回復を有効化
- `--mapping-file <path>` - カスタムフィールドマッピング
- `--preserve-ids` - 参照IDの維持
- `--archive-source` - 移行後のアーカイブ

## Examples
```bash
# Plan GitHub to Linear migration
migration-assistant plan --source github --target linear

# Analyze migration scope
migration-assistant analyze --scope all

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
  github_to_linear:
    issue:
      title: title
      body: description
      state: status
      labels: labels
      milestone: cycle
      assignees: assignees
    
    custom_fields:
      - source: "custom.priority"
        target: "priority"
        transform: "map_priority"
      
    relationships:
      - type: "parent-child"
        source: "depends_on"
        target: "parent"
    
  linear_to_github:
    issue:
      title: title
      description: body
      status: state
      priority: labels
      cycle: milestone
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

### GitHub Issues → Linear
1. Map GitHub labels to Linear labels/projects
2. Convert milestones to cycles
3. Preserve issue numbers as references
4. Migrate comments with user mapping
5. Handle attachments and images

### Linear → GitHub Issues
1. Map Linear statuses to GitHub states
2. Convert cycles to milestones
3. Preserve Linear IDs in issue body
4. Map Linear projects to labels
5. Handle custom fields

## Required MCP Servers
- mcp-server-github
- mcp-server-linear

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
This command creates a complete migration package including backups, logs, and documentation. The migration can be resumed from checkpoints in case of interruption. All migrations are reversible within the retention period.