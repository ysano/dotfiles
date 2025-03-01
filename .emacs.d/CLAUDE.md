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
- Copilot: `F2` to toggle, `<tab>` to accept completion
- Ellama: `C-c e` prefix for AI commands