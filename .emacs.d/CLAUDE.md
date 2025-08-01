# Claude Guidelines for .emacs.d

## Commands
- **Lint:** `F7` (flycheck-mode toggle)
- **PHP Tests:** `F5` (phpunit-current-test), `Shift-F5` (phpunit-current-project)
- **Go Build:** `go build -v && go test -v && go vet`
- **Git:** `C-x g` (magit-status), chord `sg` (hydra-git-gutter)

## Code Style
- **Indent:** 4 spaces (no tabs), except Terraform (2 spaces)
- **Line Length:** 80 columns
- **Encoding:** UTF-8 UNIX
- **PHP Style:** PSR-2 standard
- **Go Style:** Use gofmt (auto runs before save)
- **Python Style:** Use py-yapf (auto runs on save)
- **Ruby Style:** Use Rubocop for linting
- **Editor Config:** Enabled globally

## Navigation/Search
- `C-s` swiper for search
- `C-c g` counsel-git
- `C-c j` counsel-git-grep
- `C-c k` counsel-ag
- `C-c SPC` ace-jump-mode
- `M-o` ace-window
- `C-'` imenu-list-smart-toggle
- `C-x C-r` counsel-buffer-or-recentf

## File Navigation (counsel-find-file)
- `C-l` or `C-<backspace>` move up directory
- `C-j` select current input exactly (ivy-immediate-done)
- `RET` smart selection (ivy-alt-done)
- `C-M-y` insert current full path
- `C-o` show action menu with additional options

## Keyboard Shortcuts
- `C-=` expand region, `C--` contract region
- `C-c u` clear undo tree
- `C-x p` pop to mark
- `jk` chord for hydra-general movement

## AI Assistance
- **Claude Code Integration:**
  - `C-c C-b` send buffer for analysis
  - `C-c C-r` review selected code
  - `C-c C-f` explain function at point
  - `C-c C-q` query Claude Code
  - `C-c C-d` generate documentation

- **Advanced Claude Code Workflows (`C-c c w` prefix):**
  - `C-c c w RET` auto workflow (context-aware)
  - `C-c c w p` send project overview
  - `C-c c w g d` review git diff
  - `C-c c w g s` review staged changes
  - `C-c c w g c` suggest commit message
  - `C-c c w t g` generate tests
  - `C-c c w t f` explain test failures
  - `C-c c w q s` security review
  - `C-c c w q p` performance review
  - `C-c c w q a` accessibility review
  - `C-c c w a a` architecture analysis
  - `C-c c w a d` design patterns
  - `C-c c w D r` generate README
  - `C-c c w D a` generate API docs

- **Unified AI Tools (`C-c a` prefix):**
  - `C-c a c` Claude Code toggle
  - `C-c a r` code review with Claude
  - `C-c a f` explain function
  - `C-c a d` generate documentation
  - `C-c a R` refactor with diff preview
  - `C-c a D` show last diff
  - `C-c a h` show change history
  - `C-c a u` undo last change
  - `C-c a C` toggle Copilot mode
  - `C-c a a` accept Copilot completion
  - `C-c a l` Ellama chat
  - `C-c a t` translate text
  - `C-c a s` summarize text
  - `C-c a w` write with AI

- **Copilot:**
  - `F2` to toggle completion mode
  - `<tab>` to accept completion
  - `C-<tab>` to accept completion by word
  - `M-<tab>` to accept completion by line
  - `C-n`/`C-p` to navigate suggestions
  - `C-g` to clear overlay

- **Ellama:** `C-c e` prefix for LLM commands

## Smart Font Scaling（高解像度ディスプレイ対応）
高解像度ディスプレイ（UWQHD、4K、Retina等）での自動フォントサイズ調整システム

- **自動調整:**
  - 起動時にディスプレイDPIを検出し、最適なフォントサイズを自動設定
  - 外部ディスプレイ接続時の自動再調整
  - ミニバッファ、モードライン含む全UI要素に対応

- **手動制御:**
  - `C-c f s` 最適スケーリングを手動適用
  - `C-c f r` ディスプレイ情報を再取得して更新
  - `C-c f i` 現在の設定情報を表示
  - `C-c f m` 手動フォントサイズ設定
  - `C-c f c` 手動設定をクリア（自動に戻す）

- **現在の環境:**
  - メインディスプレイ (3440×1440): 13pt → 21pt
  - セカンダリ (2560×1440): 13pt → 17pt
  - プラットフォーム最適化フォント自動選択

詳細: `docs/smart-font-scaling.md`

## Zettelkasten（org-roam）
- **ナビゲーション:**
  - `C-c n f` タイトルでゼッテルを検索
  - `C-c n T` タグでゼッテルを検索
  - `C-c n l` バックリンクサイドバーの表示/非表示
  - `C-c n g` グラフ可視化を表示
  - `C-c n u` Web UI可視化を開く

- **作成:**
  - `C-c n c` 新規ノート作成（z:ゼッテル, r:文献, f:一時的）
  - `C-c n i` 既存ゼッテルへのリンクを挿入
  - `C-c n j` 今日の日誌を作成/開く

- **整理:**
  - `C-c n t` 現在のノートにタグを追加
  - `C-c n a` 現在のノートにエイリアスを追加
  - `C-c n s` データベースを手動で同期