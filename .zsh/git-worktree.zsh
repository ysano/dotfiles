# Git Worktree管理用コマンド群
# Claude Code複数起動とモダンな開発ワークフロー対応
# 2025年のベストプラクティスに基づく実装

# メイン関数
function gwt() {
    local command="$1"
    shift

    case "$command" in
        "create"|"c")
            _gwt_create "$@"
            ;;
        "list"|"l")
            _gwt_list "$@"
            ;;
        "switch"|"s")
            _gwt_switch "$@"
            ;;
        "remove"|"rm"|"r")
            _gwt_remove "$@"
            ;;
        "clean")
            _gwt_clean "$@"
            ;;
        "help"|"h"|"")
            _gwt_help
            ;;
        *)
            echo "❌ 不明なコマンド: $command"
            _gwt_help
            return 1
            ;;
    esac
}

# worktree作成
function _gwt_create() {
    local branch_name="$1"
    local base_branch="${2:-$(git symbolic-ref --short HEAD 2>/dev/null || echo 'main')}"

    if [[ -z "$branch_name" ]]; then
        echo "❌ ブランチ名を指定してください"
        echo "使用法: gwt create <branch-name> [base-branch]"
        return 1
    fi

    # Gitリポジトリチェック
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    # worktreeディレクトリのルートを決定
    local repo_root=$(git rev-parse --show-toplevel)
    local worktree_root="${repo_root}/../worktrees"
    local repo_name=$(basename "$repo_root")
    local worktree_dir="${worktree_root}/${repo_name}-${branch_name}"

    # worktreesディレクトリを作成
    mkdir -p "$worktree_root"

    # ブランチが既に存在するかチェック
    if git show-ref --verify --quiet "refs/heads/$branch_name"; then
        echo "📋 既存ブランチ '$branch_name' のworktreeを作成します..."
        git worktree add "$worktree_dir" "$branch_name"
    else
        echo "🌿 新しいブランチ '$branch_name' を '$base_branch' から作成します..."
        git worktree add -b "$branch_name" "$worktree_dir" "$base_branch"
    fi

    if [[ $? -eq 0 ]]; then
        echo "✅ Worktree作成完了:"
        echo "   📂 パス: $worktree_dir"
        echo "   🌿 ブランチ: $branch_name"
        echo ""
        echo "💡 Claude Codeで使用する場合:"
        echo "   cd \"$worktree_dir\" && claude"
        echo ""
        echo "🔀 切り替え: gwt switch"
    else
        echo "❌ Worktree作成に失敗しました"
        return 1
    fi
}

# worktree一覧表示
function _gwt_list() {
    local show_path="${1:-}"

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    local current_worktree=$(git rev-parse --show-toplevel)

    echo "📋 Git Worktree一覧:"
    echo ""

    git worktree list --porcelain | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            local is_current=""
            [[ "$worktree_path" == "$current_worktree" ]] && is_current=" 👈 現在の場所"

            echo -n "📂 $worktree_path$is_current"
            [[ "$show_path" == "--path" ]] && echo " ($worktree_path)" || echo ""
        elif [[ "$line" =~ ^branch ]]; then
            local branch_name="${line#branch refs/heads/}"
            echo "   🌿 ブランチ: $branch_name"
        elif [[ "$line" =~ ^HEAD ]]; then
            local commit_hash="${line#HEAD }"
            echo "   📝 コミット: ${commit_hash:0:8}"
        elif [[ "$line" == "bare" ]]; then
            echo "   📋 ベアリポジトリ"
        elif [[ -z "$line" ]]; then
            echo ""
        fi
    done

    # 利用可能なコマンドの表示
    echo "💡 利用可能なコマンド:"
    echo "   gwt switch  - worktree切り替え"
    echo "   gwt remove  - worktree削除"
    echo "   gwt clean   - メンテナンス"
}

# worktree切り替え（fzf使用）
function _gwt_switch() {
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    # fzfが利用可能かチェック
    if ! command -v fzf >/dev/null 2>&1; then
        echo "⚠️  fzfが見つかりません。通常の選択モードを使用します"
        _gwt_switch_manual
        return $?
    fi

    local current_worktree=$(git rev-parse --show-toplevel)
    local selected_worktree

    # worktree一覧を取得してfzfで選択
    selected_worktree=$(git worktree list --porcelain | \
        awk '/^worktree/ {path=$2} /^branch/ {branch=substr($0,18)} path && branch {print path " (" branch ")"; path=""; branch=""}' | \
        fzf --prompt="🔀 切り替え先のworktreeを選択: " \
            --preview='echo "📂 パス: {1}" && echo "🌿 ブランチ: {2}" | tr -d "()"' \
            --preview-window=up:2 \
            --height=40%)

    if [[ -n "$selected_worktree" ]]; then
        local worktree_path=$(echo "$selected_worktree" | awk '{print $1}')
        if [[ "$worktree_path" != "$current_worktree" ]]; then
            echo "🔀 切り替え中: $worktree_path"
            cd "$worktree_path"
            echo "✅ 切り替え完了!"
            echo "📂 現在の場所: $(pwd)"
            echo "🌿 現在のブランチ: $(git branch --show-current)"
        else
            echo "ℹ️  既に同じworktreeにいます"
        fi
    else
        echo "❌ 切り替えをキャンセルしました"
    fi
}

# 手動選択モード（fzf無し）
function _gwt_switch_manual() {
    local worktrees=($(git worktree list --porcelain | awk '/^worktree/ {print $2}'))
    local current_worktree=$(git rev-parse --show-toplevel)

    echo "📋 利用可能なworktree:"
    local i=1
    for worktree in "${worktrees[@]}"; do
        local marker=""
        [[ "$worktree" == "$current_worktree" ]] && marker=" 👈 現在"
        echo "  $i) $worktree$marker"
        ((i++))
    done

    echo -n "🔀 切り替え先の番号を入力 (1-${#worktrees[@]}): "
    read selection

    if [[ "$selection" =~ ^[0-9]+$ ]] && [[ "$selection" -ge 1 ]] && [[ "$selection" -le "${#worktrees[@]}" ]]; then
        local worktree_path="${worktrees[$selection]}"
        if [[ "$worktree_path" != "$current_worktree" ]]; then
            cd "$worktree_path"
            echo "✅ 切り替え完了: $worktree_path"
        else
            echo "ℹ️  既に同じworktreeにいます"
        fi
    else
        echo "❌ 無効な選択です"
        return 1
    fi
}

# worktree削除
function _gwt_remove() {
    local worktree_name="$1"

    if [[ -z "$worktree_name" ]]; then
        echo "❌ 削除するworktree名を指定してください"
        echo "使用法: gwt remove <worktree-name>"
        _gwt_list
        return 1
    fi

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    # worktreeが存在するかチェック
    local worktree_path=$(git worktree list --porcelain | awk -v name="$worktree_name" '/^worktree/ {path=$2} path && path~name {print path; exit}')

    if [[ -z "$worktree_path" ]]; then
        echo "❌ Worktree '$worktree_name' が見つかりません"
        _gwt_list
        return 1
    fi

    # 現在のworktreeを削除しようとしていないかチェック
    local current_worktree=$(git rev-parse --show-toplevel)
    if [[ "$worktree_path" == "$current_worktree" ]]; then
        echo "❌ 現在いるworktreeは削除できません"
        echo "他のworktreeに切り替えてから実行してください"
        return 1
    fi

    # 確認プロンプト
    echo "⚠️  削除対象:"
    echo "   📂 パス: $worktree_path"
    echo -n "本当に削除しますか？ (y/N): "
    read confirmation

    if [[ "$confirmation" =~ ^[yY]$ ]]; then
        git worktree remove "$worktree_path" --force
        if [[ $? -eq 0 ]]; then
            echo "✅ Worktree削除完了: $worktree_path"
        else
            echo "❌ Worktree削除に失敗しました"
            return 1
        fi
    else
        echo "❌ 削除をキャンセルしました"
    fi
}

# メンテナンス・クリーンアップ
function _gwt_clean() {
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    echo "🧹 Git Worktreeメンテナンスを実行中..."
    echo ""

    # 壊れたworktreeのクリーンアップ
    echo "1️⃣ 壊れたworktreeエントリをクリーンアップ中..."
    git worktree prune -v
    echo ""

    # 現在の状況表示
    echo "2️⃣ 現在のworktree状況:"
    _gwt_list
    echo ""

    # ディスク使用量チェック
    echo "3️⃣ ディスク使用量:"
    local repo_root=$(git rev-parse --show-toplevel)
    local worktree_root="${repo_root}/../worktrees"

    if [[ -d "$worktree_root" ]]; then
        du -sh "$worktree_root"/* 2>/dev/null | sort -h || echo "worktreeディレクトリが空です"
    else
        echo "worktreeディレクトリはまだ作成されていません"
    fi
    echo ""

    echo "✅ メンテナンス完了"
}

# ヘルプ表示
function _gwt_help() {
    cat << 'EOF'
🚀 Git Worktree管理コマンド (Claude Code対応版)

📋 使用法:
  gwt create <branch-name> [base-branch]  新しいworktreeを作成
  gwt list                                worktree一覧を表示
  gwt switch                              worktree切り替え (fzf対応)
  gwt remove <worktree-name>              worktreeを削除
  gwt clean                               メンテナンス・クリーンアップ
  gwt help                                このヘルプを表示

🔧 短縮エイリアス:
  gwt c    ≡ gwt create
  gwt l    ≡ gwt list
  gwt s    ≡ gwt switch
  gwt rm   ≡ gwt remove

💡 Claude Code連携の使用例:
  # 機能開発用worktree作成
  gwt create feature/user-auth main

  # worktree切り替えしてClaude Code起動
  gwt switch
  claude

  # 開発完了後のクリーンアップ
  gwt remove feature/user-auth
  gwt clean

🎯 2025年ベストプラクティス対応:
  ✅ 一貫した命名規則とディレクトリ構造
  ✅ アクティブタスクのみでworktree管理
  ✅ 自動クリーンアップとメンテナンス
  ✅ fzfによるインタラクティブ操作
  ✅ チーム協力に配慮した安全な設計

EOF
}

# 補完機能
function _gwt_completion() {
    local commands="create list switch remove clean help c l s rm r h"
    local current_word="${COMP_WORDS[COMP_CWORD]}"
    local prev_word="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev_word" in
        "gwt")
            COMPREPLY=($(compgen -W "$commands" -- "$current_word"))
            ;;
        "remove"|"rm"|"r")
            # worktree名の補完
            if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
                local worktrees=$(git worktree list --porcelain | awk '/^worktree/ {print $2}' | xargs -I {} basename {})
                COMPREPLY=($(compgen -W "$worktrees" -- "$current_word"))
            fi
            ;;
        "create"|"c")
            # ブランチ名の補完
            if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
                local branches=$(git branch -r --format='%(refname:short)' | sed 's/origin\///')
                COMPREPLY=($(compgen -W "$branches" -- "$current_word"))
            fi
            ;;
    esac
}

# Zsh補完の設定
if [[ -n "$ZSH_VERSION" ]]; then
    autoload -U compinit
    compinit

    # Zsh補完関数
    function _gwt() {
        local context state line
        local -a commands

        commands=(
            'create:新しいworktreeを作成'
            'list:worktree一覧を表示'
            'switch:worktree切り替え'
            'remove:worktreeを削除'
            'clean:メンテナンス・クリーンアップ'
            'help:ヘルプを表示'
        )

        _arguments \
            '1: :->command' \
            '*: :->args'

        case $state in
            command)
                _describe 'commands' commands
                ;;
            args)
                case $words[2] in
                    create|c)
                        if [[ $#words -eq 3 ]]; then
                            _message 'ブランチ名'
                        elif [[ $#words -eq 4 ]]; then
                            _git_branches
                        fi
                        ;;
                    remove|rm|r)
                        if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
                            local worktrees=($(git worktree list --porcelain | awk '/^worktree/ {print $2}' | xargs -I {} basename {}))
                            _describe 'worktrees' worktrees
                        fi
                        ;;
                esac
                ;;
        esac
    }

    compdef _gwt gwt
fi

# Bash補完の設定
if [[ -n "$BASH_VERSION" ]]; then
    complete -F _gwt_completion gwt
fi
