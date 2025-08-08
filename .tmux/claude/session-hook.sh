#!/bin/bash
# Claude Voice Session Hook - Smart Integration Point
# セッション作成時の智的統合処理

# Claude Voice統合の前提条件チェック
check_claude_integration() {
    local config_file="$HOME/.tmux/claude/config/integration.conf"

    # 設定ファイルの存在確認
    if [[ ! -f "$config_file" ]]; then
        return 1
    fi

    # 統合有効化フラグの確認
    if ! grep -q "^enabled=true" "$config_file" 2>/dev/null; then
        return 1
    fi

    # 必要なコンポーネントの確認
    if [[ ! -x "$HOME/.tmux/claude/bin/claude-voice" ]]; then
        return 1
    fi

    return 0
}

# グローバル初期化処理（セッション分離機能を簡素化）
initialize_claude_global() {
    # グローバル設定ディレクトリの確認
    mkdir -p "$HOME/.tmux/claude/global"

    # 初期化ログ（セッションIDは記録するが分離はしない）
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Claude integration initialized" >> \
        "$HOME/.tmux/claude/logs/global.log"

    # 初回ウェルカム通知（設定に基づく）
    if grep -q "^welcome_notification=true" "$HOME/.tmux/claude/config/integration.conf" 2>/dev/null; then
        # バックグラウンドで短いウェルカム音声を再生
        (echo "Claude Voice統合が有効になりました。" |
            "$HOME/.tmux/claude/bin/claude-voice" --text-mode 2>/dev/null &)
    fi
}

# メイン処理
main() {
    if check_claude_integration; then
        initialize_claude_global
    fi
}

# スクリプト実行（直接呼び出し時のみ）
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
