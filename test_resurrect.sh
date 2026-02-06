#!/usr/bin/env bash
# tmux resurrect/continuum 動作確認スクリプト

set -e

echo "=== tmux resurrect/continuum 動作確認 ==="
echo ""

# 1. プラグインのインストール確認
echo "1. プラグインのインストール確認"
if [ -d "$HOME/.tmux/plugins/tmux-resurrect" ] && [ -d "$HOME/.tmux/plugins/tmux-continuum" ]; then
    echo "   ✅ プラグインがインストールされています"
    echo "      - tmux-resurrect: $(ls -ld ~/.tmux/plugins/tmux-resurrect | awk '{print $6, $7, $8}')"
    echo "      - tmux-continuum: $(ls -ld ~/.tmux/plugins/tmux-continuum | awk '{print $6, $7, $8}')"
else
    echo "   ❌ プラグインがインストールされていません"
    exit 1
fi
echo ""

# 2. 設定ファイルの確認
echo "2. 設定ファイルの確認"
if [ -f "$HOME/dotfiles/.tmux/plugin-config/resurrect.conf" ]; then
    echo "   ✅ resurrect.conf が存在します"
    echo "      場所: $HOME/dotfiles/.tmux/plugin-config/resurrect.conf"
else
    echo "   ❌ resurrect.conf が存在しません"
    exit 1
fi
echo ""

# 3. .tmux.confの確認
echo "3. .tmux.confの統合確認"
if grep -q "source-file ~/.tmux/plugin-config/resurrect.conf" "$HOME/dotfiles/.tmux.conf"; then
    echo "   ✅ resurrect.confが読み込まれています"
else
    echo "   ❌ resurrect.confが読み込まれていません"
    exit 1
fi
if grep -q "run '~/.tmux/plugins/tpm/tpm'" "$HOME/dotfiles/.tmux.conf"; then
    echo "   ✅ TPMが初期化されています"
else
    echo "   ❌ TPMが初期化されていません"
    exit 1
fi
echo ""

# 4. tmux設定の確認（tmuxが実行中の場合）
echo "4. tmux設定の確認"
if tmux info &>/dev/null; then
    echo "   ✅ tmuxが実行中です"

    # resurrect設定の確認
    if tmux show-options -g | grep -q "@resurrect-processes"; then
        echo "   ✅ resurrect設定が読み込まれています"
        RESURRECT_PROCS=$(tmux show-options -g @resurrect-processes | cut -d' ' -f2-)
        echo "      復元対象: $RESURRECT_PROCS"
    else
        echo "   ⚠️  resurrect設定が読み込まれていません（tmux再起動が必要かもしれません）"
    fi

    # continuum設定の確認
    if tmux show-options -g | grep -q "@continuum-save-interval"; then
        echo "   ✅ continuum設定が読み込まれています"
        SAVE_INTERVAL=$(tmux show-options -g @continuum-save-interval | cut -d' ' -f2)
        echo "      自動保存間隔: ${SAVE_INTERVAL}分"

        AUTO_RESTORE=$(tmux show-options -g @continuum-restore | cut -d' ' -f2)
        echo "      自動復元: $AUTO_RESTORE"
    else
        echo "   ⚠️  continuum設定が読み込まれていません（tmux再起動が必要かもしれません）"
    fi

    # status-rightの確認
    STATUS_RIGHT=$(tmux show-options -g status-right | cut -d' ' -f2-)
    if echo "$STATUS_RIGHT" | grep -q "continuum_status"; then
        echo "   ✅ status-rightにcontinuum_statusが統合されています"
    else
        echo "   ⚠️  status-rightにcontinuum_statusが見つかりません"
    fi
else
    echo "   ⚠️  tmuxが実行されていません"
    echo "      tmuxを起動してから再度このスクリプトを実行してください"
fi
echo ""

# 5. 保存ディレクトリの確認
echo "5. 保存ディレクトリの確認"
if [ -d "$HOME/.local/share/tmux-resurrect" ]; then
    echo "   ✅ 保存ディレクトリが存在します"
    SAVE_COUNT=$(ls -1 "$HOME/.local/share/tmux-resurrect" 2>/dev/null | wc -l | tr -d ' ')
    echo "      保存ファイル数: $SAVE_COUNT"
    if [ -L "$HOME/.local/share/tmux-resurrect/last" ]; then
        LAST_SAVE=$(readlink "$HOME/.local/share/tmux-resurrect/last")
        echo "      最新の保存: $LAST_SAVE"
    fi
else
    echo "   ℹ️  保存ディレクトリはまだ作成されていません"
    echo "      （初回保存時に自動作成されます）"
fi
echo ""

# 6. キーバインドの確認
echo "6. キーバインドの確認"
if tmux info &>/dev/null; then
    echo "   手動保存: Prefix + Ctrl-s"
    echo "   手動復元: Prefix + Ctrl-r"
    echo ""
    echo "   ※ tmux内で実際に動作するか確認してください"
else
    echo "   ⚠️  tmuxが実行されていないため確認できません"
fi
echo ""

echo "=== 検証完了 ==="
echo ""
echo "次のステップ:"
echo "1. tmuxを起動（または既存セッションに接続）: tmux"
echo "2. 複数のウィンドウとペインを作成してテスト"
echo "3. 手動保存を試す: Prefix + Ctrl-s"
echo "4. tmuxを終了: exit"
echo "5. tmuxを再起動して自動復元を確認: tmux"
echo ""
