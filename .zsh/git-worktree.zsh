# Git Worktree管理コマンド (gwt)
# コア操作のみ: create, list, switch, remove, clean

# エイリアス競合解決（Oh My Zsh git plugin対応）
unalias gwt gwta gwtls gwtmv gwtrm 2>/dev/null || true

# ================================================================================
# ユーティリティ
# ================================================================================

# worktreeディレクトリパスを計算
# ブランチ名のスラッシュはダッシュに変換
_gwt_worktree_dir() {
    local branch_name="$1"
    local repo_root=$(git rev-parse --show-toplevel)
    local repo_name=$(basename "$repo_root")
    local safe_name=$(echo "$branch_name" | tr '/' '-')
    echo "${repo_root}/../worktrees/${repo_name}-${safe_name}"
}

# Gitリポジトリ内かチェック
_gwt_require_repo() {
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi
}

# ================================================================================
# メインディスパッチ
# ================================================================================

gwt() {
    local command="$1"
    shift 2>/dev/null

    case "$command" in
        create|c)    _gwt_create "$@" ;;
        list|l)      _gwt_list "$@" ;;
        switch|s)    _gwt_switch "$@" ;;
        remove|rm|r) _gwt_remove "$@" ;;
        clean)       _gwt_clean ;;
        help|h|"")   _gwt_help ;;
        *)
            echo "❌ 不明なコマンド: $command"
            _gwt_help
            return 1
            ;;
    esac
}

# ================================================================================
# サブコマンド
# ================================================================================

_gwt_create() {
    local switch_after=false
    local branch_name=""
    local base_branch=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -s|--switch) switch_after=true; shift ;;
            -*) echo "使用法: gwt create [-s] <branch-name> [base-branch]"; return 1 ;;
            *)
                if [[ -z "$branch_name" ]]; then
                    branch_name="$1"
                elif [[ -z "$base_branch" ]]; then
                    base_branch="$1"
                fi
                shift
                ;;
        esac
    done

    if [[ -z "$branch_name" ]]; then
        echo "使用法: gwt create [-s] <branch-name> [base-branch]"
        return 1
    fi

    _gwt_require_repo || return 1

    [[ -z "$base_branch" ]] && base_branch=$(git symbolic-ref --short HEAD 2>/dev/null || echo 'main')

    local worktree_dir=$(_gwt_worktree_dir "$branch_name")
    mkdir -p "$(dirname "$worktree_dir")"

    if git show-ref --verify --quiet "refs/heads/$branch_name"; then
        echo "📋 既存ブランチ '$branch_name' のworktreeを作成..."
        git worktree add "$worktree_dir" "$branch_name"
    else
        echo "🌿 '$branch_name' を '$base_branch' から作成..."
        git worktree add -b "$branch_name" "$worktree_dir" "$base_branch"
    fi

    if [[ $? -eq 0 ]]; then
        echo "✅ $worktree_dir"
        if [[ "$switch_after" == true ]]; then
            cd "$worktree_dir"
            echo "📂 $(pwd)"
        else
            echo "💡 cd \"$worktree_dir\""
        fi
    else
        echo "❌ 作成失敗"
        return 1
    fi
}

_gwt_list() {
    _gwt_require_repo || return 1

    local current=$(git rev-parse --show-toplevel)

    git worktree list | while IFS= read -r line; do
        local wt_path=${line%% *}
        if [[ "$wt_path" == "$current" ]]; then
            echo "👈 $line"
        else
            echo "   $line"
        fi
    done
}

_gwt_switch() {
    _gwt_require_repo || return 1

    if ! command -v fzf >/dev/null 2>&1; then
        echo "❌ fzfが必要です: brew install fzf"
        return 1
    fi

    local current=$(git rev-parse --show-toplevel)
    local selected=$(git worktree list | fzf --prompt="worktree> " --height=40% | awk '{print $1}')

    if [[ -n "$selected" && "$selected" != "$current" ]]; then
        cd "$selected"
        echo "✅ $(git branch --show-current) @ $selected"
    elif [[ "$selected" == "$current" ]]; then
        echo "ℹ️  既に現在のworktreeです"
    fi
}

_gwt_remove() {
    local force=false
    local delete_branch=false
    local worktree_name=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -d|--delete-branch) delete_branch=true; shift ;;
            -f|--force) force=true; shift ;;
            -*) echo "使用法: gwt remove [-d] [-f] <worktree-name>"; return 1 ;;
            *) worktree_name="$1"; shift ;;
        esac
    done

    if [[ -z "$worktree_name" ]]; then
        echo "使用法: gwt remove [-d] [-f] <worktree-name>"
        return 1
    fi

    _gwt_require_repo || return 1

    # worktreeパスまたはブランチ名で部分一致検索
    local -a matches
    local safe_name=$(echo "$worktree_name" | tr '/' '-')
    local wt_path="" wt_branch=""
    while IFS= read -r line; do
        if [[ "$line" == worktree\ * ]]; then
            wt_path="${line#worktree }"
            wt_branch=""
        elif [[ "$line" == branch\ * ]]; then
            wt_branch="${line#branch refs/heads/}"
            # パス部分一致 or ブランチ名一致
            if [[ "$wt_path" == *"$worktree_name"* ]] || \
               [[ "$wt_path" == *"$safe_name"* ]] || \
               [[ "$wt_branch" == "$worktree_name" ]]; then
                matches+=("$wt_path")
            fi
        fi
    done < <(git worktree list --porcelain)

    if [[ ${#matches[@]} -eq 0 ]]; then
        echo "❌ '$worktree_name' が見つかりません"
        gwt list
        return 1
    elif [[ ${#matches[@]} -gt 1 ]]; then
        echo "❌ 複数のworktreeがマッチ:"
        printf "   %s\n" "${matches[@]}"
        return 1
    fi

    local worktree_path="${matches[1]}"
    local current=$(git rev-parse --show-toplevel)

    if [[ "$worktree_path" == "$current" ]]; then
        echo "❌ 現在のworktreeは削除できません"
        return 1
    fi

    # ブランチ名取得
    local branch_name=$(git worktree list --porcelain | \
        grep -A 2 "^worktree ${worktree_path}$" | \
        awk '/^branch/ {sub("refs/heads/", ""); print $2}')

    # 削除実行
    local -a remove_args
    [[ "$force" == true ]] && remove_args+=(--force)

    git worktree remove "${remove_args[@]}" "$worktree_path" || return 1
    echo "✅ worktree削除: $worktree_path"

    if [[ "$delete_branch" == true && -n "$branch_name" ]]; then
        local branch_flag="-d"
        [[ "$force" == true ]] && branch_flag="-D"
        git branch "$branch_flag" "$branch_name" 2>/dev/null && echo "✅ ブランチ削除: $branch_name"
    fi
}

_gwt_clean() {
    _gwt_require_repo || return 1

    echo "🧹 worktree prune..."
    git worktree prune -v
    echo ""
    gwt list
}

_gwt_help() {
    cat <<'EOF'
gwt - Git Worktree管理

使用法:
  gwt create [-s] <branch> [base]   worktree作成 (-s: 作成後に移動)
  gwt list                          一覧表示
  gwt switch                        fzfで切り替え
  gwt remove [-d] [-f] <name>       削除 (-d: ブランチも, -f: 強制)
  gwt clean                         クリーンアップ
  gwt help                          このヘルプ

エイリアス:
  gw=gwt  gwc=create  gwl=list  gwsw=switch  gwr=remove
EOF
}

# ================================================================================
# Zsh補完
# ================================================================================

if [[ -n "$ZSH_VERSION" ]]; then
    _gwt() {
        local -a commands
        commands=(
            'create:worktree作成'
            'list:一覧表示'
            'switch:fzfで切り替え'
            'remove:削除'
            'clean:クリーンアップ'
            'help:ヘルプ'
        )
        _arguments '1: :->command' '*: :->args'
        case $state in
            command) _describe 'commands' commands ;;
            args)
                case $words[2] in
                    create|c)
                        [[ $#words -eq 3 ]] && _message 'branch-name' || _git_branches
                        ;;
                    remove|rm|r)
                        if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
                            local -a wts
                            wts=($(git worktree list --porcelain 2>/dev/null | awk '/^worktree/ {print $2}' | while read p; do basename "$p"; done))
                            _describe 'worktrees' wts
                        fi
                        ;;
                esac
                ;;
        esac
    }
    compdef _gwt gwt 2>/dev/null
fi

# ================================================================================
# エイリアス
# ================================================================================

alias gw='gwt'
alias gwc='gwt create'
alias gwl='gwt list'
alias gwsw='gwt switch'
alias gwr='gwt remove'
