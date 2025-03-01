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

## Keyboard Shortcuts
- `C-=` expand region, `C--` contract region
- `C-c u` clear undo tree
- `C-x p` pop to mark
- `jk` chord for hydra-general movement

## AI Assistance
- **Copilot:**
  - `F2` to toggle completion mode
  - `<tab>` to accept completion
  - `C-<tab>` to accept completion by word
  - `M-<tab>` to accept completion by line
  - `C-n`/`C-p` to navigate suggestions
  - `C-g` to clear overlay
- **Ellama:** `C-c e` prefix for LLM commands

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