#!/bin/bash
# .emacs.d/inits/init-dev-core.el の id-manager 設定の検証
# idm-database-file はパッケージ側で defvar のため、use-package の :custom では
# 遅延ロード後に既定値 "~/.idm-db.gpg" へ戻ってしまう（M-7 が空の新規 DB を開く）。
# 使い方: bash test_emacs_idm.sh   （emacs が必要。未導入ならスキップ）
set -u
INIT="$(cd "$(dirname "$0")" && pwd)/.emacs.d/inits/init-dev-core.el"
command -v emacs >/dev/null 2>&1 || { echo "skip emacs が無い"; exit 0; }

work=$(mktemp -d) && [ -d "$work" ] || { echo "FAIL mktemp -d"; exit 1; }
trap 'rm -rf "$work"' EXIT
# 本物のパッケージの代わりに、同じ defvar を持つスタブを置く（ネットワーク・elpa 不要）
cat > "$work/id-manager.el" <<'EOF'
(defvar idm-database-file "~/.idm-db.gpg")
(defun id-manager () (interactive))
(provide 'id-manager)
EOF

# init-dev-core.el から (use-package id-manager ...) だけを読み出して評価し、ロード後の値を出す
out=$(emacs --batch -Q -L "$work" --eval "
(progn
  (require 'use-package)
  (setq use-package-always-ensure nil use-package-ensure-function #'ignore)
  (with-temp-buffer
    (insert-file-contents \"$INIT\")
    (goto-char (point-min))
    (search-forward \"(use-package id-manager\")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t))
  (require 'id-manager)
  (princ (format \"%s\n\" idm-database-file)))" 2>/dev/null | tail -1)

expected="~/secret/idm-db.gpg"
if [ "$out" = "$expected" ]; then
    echo "ok   ロード後の idm-database-file: $out"
else
    echo "FAIL ロード後の idm-database-file: 期待 $expected / 実際 ${out:-(出力なし)}"
    exit 1
fi
