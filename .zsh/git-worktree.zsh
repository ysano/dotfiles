# Git Worktree管理用コマンド群
# Claude Code複数起動とモダンな開発ワークフロー対応
# 2025年のベストプラクティスに基づく実装

# エイリアス競合の解決: 先にエイリアスを削除（Oh My Zsh git plugin対応）
unalias gwt gwta gwtls gwtmv gwtrm 2>/dev/null || true

# メイン関数
function gwt() {
    # 設定読み込み
    _gwt_load_config
    
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
        "sync")
            _gwt_sync "$@"
            ;;
        "exec")
            _gwt_exec "$@"
            ;;
        "status")
            _gwt_status "$@"
            ;;
        "open"|"o")
            _gwt_open "$@"
            ;;
        "config")
            _gwt_config "$@"
            ;;
        "pr")
            _gwt_pr "$@"
            ;;
        "env")
            _gwt_env "$@"
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
    local switch_after=false
    local branch_name=""
    local base_branch=""
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -s|--switch)
                switch_after=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt create [-s|--switch] <branch-name> [base-branch]"
                return 1
                ;;
            *)
                if [[ -z "$branch_name" ]]; then
                    branch_name="$1"
                elif [[ -z "$base_branch" ]]; then
                    base_branch="$1"
                else
                    echo "❌ 引数が多すぎます"
                    echo "使用法: gwt create [-s|--switch] <branch-name> [base-branch]"
                    return 1
                fi
                shift
                ;;
        esac
    done

    # デフォルトベースブランチの設定
    if [[ -z "$base_branch" ]]; then
        base_branch="$(git symbolic-ref --short HEAD 2>/dev/null || echo 'main')"
    fi

    if [[ -z "$branch_name" ]]; then
        echo "❌ ブランチ名を指定してください"
        echo "使用法: gwt create [-s|--switch] <branch-name> [base-branch]"
        echo ""
        echo "オプション:"
        echo "  -s, --switch    作成後に新しいworktreeディレクトリに移動"
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
        
        # 環境ファイルの自動セットアップを試行
        echo "🔍 環境ファイルのセットアップを確認中..."
        local template_files=($(_gwt_detect_env_files "$repo_root" | grep '\.example$'))
        
        if [[ ${#template_files[@]} -gt 0 ]]; then
            echo "   📋 テンプレートファイル発見: ${#template_files[@]}個"
            
            # 自動セットアップを実行
            echo "   🛠️  環境ファイルを自動セットアップ中..."
            local original_pwd=$(pwd)
            cd "$worktree_dir"
            
            for template in "${template_files[@]}"; do
                local template_name=$(basename "$template")
                echo "      📄 $template_name から環境ファイルを作成中..."
                
                # 既存の環境ファイルがない場合のみセットアップ
                local target_name="${template_name%.example}"
                if [[ ! -f "$target_name" ]]; then
                    # ポート自動調整付きでセットアップ
                    gwt env setup --template "$template" --target "$worktree_dir" --auto-port >/dev/null 2>&1
                    if [[ $? -eq 0 ]]; then
                        echo "      ✅ $target_name 作成完了"
                    else
                        echo "      ⚠️  $target_name の作成をスキップ"
                    fi
                else
                    echo "      ℹ️  $target_name は既に存在します"
                fi
            done
            
            cd "$original_pwd"
            echo "   ✅ 環境ファイルセットアップ完了"
        else
            echo "   ℹ️  環境ファイルテンプレートが見つかりませんでした"
        fi
        
        echo ""
        
        if [[ "$switch_after" == true ]]; then
            echo "📂 新しいworktreeディレクトリに移動しています..."
            cd "$worktree_dir"
            echo "✅ 現在のディレクトリ: $(pwd)"
            echo ""
            echo "💡 Claude Codeを起動する場合: claude"
        else
            echo "💡 Claude Codeで使用する場合:"
            echo "   cd \"$worktree_dir\" && claude"
            echo ""
            echo "🔀 切り替え: gwt switch"
        fi
    else
        echo "❌ Worktree作成に失敗しました"
        return 1
    fi
}

# worktree一覧表示
function _gwt_list() {
    local verbose=false
    local show_path=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -v|--verbose)
                verbose=true
                shift
                ;;
            --path)
                show_path=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt list [-v|--verbose] [--path]"
                return 1
                ;;
            *)
                echo "❌ 不明な引数: $1"
                echo "使用法: gwt list [-v|--verbose] [--path]"
                return 1
                ;;
        esac
    done

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    local current_worktree=$(git rev-parse --show-toplevel)

    if [[ "$verbose" == true ]]; then
        echo "📋 Git Worktree詳細一覧:"
    else
        echo "📋 Git Worktree一覧:"
    fi
    echo ""

    git worktree list --porcelain | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            local is_current=""
            [[ "$worktree_path" == "$current_worktree" ]] && is_current=" 👈 現在の場所"

            echo -n "📂 $worktree_path$is_current"
            [[ "$show_path" == true ]] && echo " ($worktree_path)" || echo ""
            
            # verboseモードでの追加情報表示
            if [[ "$verbose" == true ]] && [[ -d "$worktree_path" ]]; then
                # ディスク使用量
                local disk_usage=$(du -sh "$worktree_path" 2>/dev/null | cut -f1)
                echo "   💾 サイズ: $disk_usage"
                
                # Git status情報
                local original_pwd=$(pwd)
                cd "$worktree_path" 2>/dev/null
                if [[ $? -eq 0 ]]; then
                    local status_output=$(git status --porcelain 2>/dev/null)
                    if [[ -n "$status_output" ]]; then
                        local modified_count=$(echo "$status_output" | grep -c "^ M" || echo "0")
                        local added_count=$(echo "$status_output" | grep -c "^A" || echo "0")
                        local untracked_count=$(echo "$status_output" | grep -c "^??" || echo "0")
                        echo "   📊 変更: M:$modified_count A:$added_count ?:$untracked_count"
                    else
                        echo "   ✅ クリーン状態"
                    fi
                    
                    # 最新コミット情報
                    local last_commit=$(git log -1 --format="%h %s" 2>/dev/null)
                    if [[ -n "$last_commit" ]]; then
                        echo "   🕒 最新: $last_commit"
                    fi
                    
                    # リモート同期状況
                    local ahead_behind=$(git rev-list --left-right --count HEAD...@{upstream} 2>/dev/null)
                    if [[ -n "$ahead_behind" ]]; then
                        local ahead=$(echo "$ahead_behind" | cut -f1)
                        local behind=$(echo "$ahead_behind" | cut -f2)
                        if [[ "$ahead" -gt 0 ]] || [[ "$behind" -gt 0 ]]; then
                            echo "   🔄 同期: +$ahead -$behind"
                        else
                            echo "   ✅ 同期済み"
                        fi
                    fi
                fi
                cd "$original_pwd" 2>/dev/null
            fi
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
    if [[ "$verbose" == false ]]; then
        echo "   gwt list -v - 詳細情報表示"
    fi
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
    local delete_branch=false
    local force=false
    local worktree_name=""
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -d|--delete-branch)
                delete_branch=true
                shift
                ;;
            --force)
                force=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt remove [-d|--delete-branch] [--force] <worktree-name>"
                return 1
                ;;
            *)
                if [[ -z "$worktree_name" ]]; then
                    worktree_name="$1"
                else
                    echo "❌ 引数が多すぎます"
                    echo "使用法: gwt remove [-d|--delete-branch] [--force] <worktree-name>"
                    return 1
                fi
                shift
                ;;
        esac
    done

    if [[ -z "$worktree_name" ]]; then
        echo "❌ 削除するworktree名を指定してください"
        echo "使用法: gwt remove [-d|--delete-branch] [--force] <worktree-name>"
        echo ""
        echo "オプション:"
        echo "  -d, --delete-branch  対応するブランチも一緒に削除"
        echo "  --force              未コミット変更があっても強制削除"
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

    # ブランチ名を取得
    local branch_name=""
    local branch_info=$(git worktree list --porcelain | grep -A 2 "^worktree $worktree_path" | grep "^branch")
    if [[ -n "$branch_info" ]]; then
        branch_name="${branch_info#branch refs/heads/}"
    fi

    # 現在のworktreeを削除しようとしていないかチェック
    local current_worktree=$(git rev-parse --show-toplevel)
    if [[ "$worktree_path" == "$current_worktree" ]]; then
        echo "❌ 現在いるworktreeは削除できません"
        echo "他のworktreeに切り替えてから実行してください"
        return 1
    fi

    # 安全性チェック
    if [[ "$delete_branch" == true ]] && [[ -n "$branch_name" ]] && [[ "$force" == false ]]; then
        # ブランチがマージされているかチェック
        local main_branch=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main")
        local merge_check=$(git branch --merged "$main_branch" | grep -c "^ *$branch_name$" || echo "0")
        
        if [[ "$merge_check" -eq 0 ]]; then
            echo "⚠️  警告: ブランチ '$branch_name' は '$main_branch' にマージされていません"
            echo "未マージのブランチを削除すると、変更が失われる可能性があります"
            echo ""
            echo "強制削除する場合は --force オプションを使用してください"
            echo "コマンド: gwt remove --delete-branch --force $worktree_name"
            return 1
        fi
    fi

    # 確認プロンプト
    echo "⚠️  削除対象:"
    echo "   📂 パス: $worktree_path"
    if [[ "$delete_branch" == true ]] && [[ -n "$branch_name" ]]; then
        echo "   🌿 ブランチ: $branch_name (削除されます)"
    fi
    echo -n "本当に削除しますか？ (y/N): "
    read confirmation

    if [[ "$confirmation" =~ ^[yY]$ ]]; then
        # worktree削除
        local remove_args="$worktree_path"
        [[ "$force" == true ]] && remove_args="$remove_args --force"
        
        git worktree remove $remove_args
        if [[ $? -eq 0 ]]; then
            echo "✅ Worktree削除完了: $worktree_path"
            
            # ブランチ削除
            if [[ "$delete_branch" == true ]] && [[ -n "$branch_name" ]]; then
                local branch_delete_args="-d"
                [[ "$force" == true ]] && branch_delete_args="-D"
                
                git branch $branch_delete_args "$branch_name"
                if [[ $? -eq 0 ]]; then
                    echo "✅ ブランチ削除完了: $branch_name"
                else
                    echo "⚠️  ブランチ削除に失敗しました: $branch_name"
                    echo "手動で削除してください: git branch -D $branch_name"
                fi
            fi
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

# 全worktree同期
function _gwt_sync() {
    local dry_run=false
    local rebase=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dry-run)
                dry_run=true
                shift
                ;;
            --rebase)
                rebase=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt sync [--dry-run] [--rebase]"
                return 1
                ;;
            *)
                echo "❌ 不明な引数: $1"
                echo "使用法: gwt sync [--dry-run] [--rebase]"
                return 1
                ;;
        esac
    done

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    local current_worktree=$(git rev-parse --show-toplevel)
    local original_pwd=$(pwd)
    local main_branch=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main")
    
    echo "🔄 Git Worktree同期を開始..."
    echo "   基底ブランチ: $main_branch"
    [[ "$dry_run" == true ]] && echo "   モード: ドライラン（実際の変更は行いません）"
    [[ "$rebase" == true ]] && echo "   同期方法: rebase" || echo "   同期方法: merge"
    echo ""

    # リモートの最新情報を取得
    echo "1️⃣ リモートの最新情報を取得中..."
    if [[ "$dry_run" == false ]]; then
        git fetch --all --prune
        if [[ $? -ne 0 ]]; then
            echo "❌ リモート情報の取得に失敗しました"
            return 1
        fi
        echo "✅ リモート情報の取得完了"
    else
        echo "🔍 ドライラン: git fetch --all --prune"
    fi
    echo ""

    # 各worktreeを処理
    echo "2️⃣ 各worktreeの同期を実行..."
    echo ""
    
    local worktree_count=0
    local sync_count=0
    local error_count=0
    
    git worktree list --porcelain | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            local branch_name=""
            local is_current=""
            
            # ブランチ名を取得（次の行を読む）
            read -r branch_line
            if [[ "$branch_line" =~ ^branch ]]; then
                branch_name="${branch_line#branch refs/heads/}"
            fi
            
            [[ "$worktree_path" == "$current_worktree" ]] && is_current=" 👈 現在"
            
            echo "📂 $worktree_path$is_current"
            echo "   🌿 ブランチ: $branch_name"
            
            worktree_count=$((worktree_count + 1))
            
            # worktreeディレクトリに移動
            if [[ -d "$worktree_path" ]]; then
                cd "$worktree_path" 2>/dev/null
                if [[ $? -ne 0 ]]; then
                    echo "   ❌ ディレクトリへの移動に失敗"
                    error_count=$((error_count + 1))
                    continue
                fi
                
                # 現在の状態チェック
                local status_output=$(git status --porcelain 2>/dev/null)
                if [[ -n "$status_output" ]]; then
                    echo "   ⚠️  未コミットの変更があります（スキップ）"
                    continue
                fi
                
                # ブランチが存在し、リモートとの関連がある場合のみ同期
                if [[ -n "$branch_name" ]] && git rev-parse --verify "origin/$branch_name" >/dev/null 2>&1; then
                    local ahead_behind=$(git rev-list --left-right --count HEAD...origin/$branch_name 2>/dev/null)
                    if [[ -n "$ahead_behind" ]]; then
                        local ahead=$(echo "$ahead_behind" | cut -f1)
                        local behind=$(echo "$ahead_behind" | cut -f2)
                        
                        if [[ "$behind" -gt 0 ]]; then
                            echo "   📥 リモートから$behind件の更新を取得"
                            if [[ "$dry_run" == false ]]; then
                                if [[ "$rebase" == true ]]; then
                                    git rebase "origin/$branch_name"
                                else
                                    git merge "origin/$branch_name"
                                fi
                                
                                if [[ $? -eq 0 ]]; then
                                    echo "   ✅ 同期完了"
                                    sync_count=$((sync_count + 1))
                                else
                                    echo "   ❌ 同期に失敗（コンフリクトの可能性）"
                                    error_count=$((error_count + 1))
                                fi
                            else
                                echo "   🔍 ドライラン: git $(if [[ "$rebase" == true ]]; then echo "rebase"; else echo "merge"; fi) origin/$branch_name"
                            fi
                        elif [[ "$ahead" -gt 0 ]]; then
                            echo "   📤 ローカルに$ahead件の未プッシュコミット"
                        else
                            echo "   ✅ 既に最新"
                        fi
                    fi
                else
                    echo "   ℹ️  リモートブランチが存在しません"
                fi
                
                # 基底ブランチからの更新チェック
                if [[ "$branch_name" != "$main_branch" ]] && git rev-parse --verify "origin/$main_branch" >/dev/null 2>&1; then
                    local main_ahead_behind=$(git rev-list --left-right --count HEAD...origin/$main_branch 2>/dev/null)
                    if [[ -n "$main_ahead_behind" ]]; then
                        local main_behind=$(echo "$main_ahead_behind" | cut -f2)
                        if [[ "$main_behind" -gt 0 ]]; then
                            echo "   📊 $main_branch から$main_behind件の更新が利用可能"
                        fi
                    fi
                fi
            else
                echo "   ❌ worktreeディレクトリが存在しません"
                error_count=$((error_count + 1))
            fi
            echo ""
        fi
    done
    
    # 元のディレクトリに戻る
    cd "$original_pwd" 2>/dev/null
    
    # サマリー表示
    echo "📊 同期結果サマリー:"
    echo "   📂 処理したworktree数: $worktree_count"
    echo "   ✅ 同期成功数: $sync_count"
    echo "   ❌ エラー数: $error_count"
    echo ""
    
    if [[ "$error_count" -gt 0 ]]; then
        echo "⚠️  エラーが発生したworktreeがあります。手動で確認してください。"
        return 1
    else
        echo "✅ 全ての同期が完了しました"
    fi
}

# 全worktreeでコマンド実行
function _gwt_exec() {
    local parallel=false
    local dry_run=false
    local continue_on_error=false
    local command_to_exec=""
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -p|--parallel)
                parallel=true
                shift
                ;;
            --dry-run)
                dry_run=true
                shift
                ;;
            -c|--continue)
                continue_on_error=true
                shift
                ;;
            --)
                shift
                command_to_exec="$*"
                break
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt exec [-p|--parallel] [--dry-run] [-c|--continue] [--] <command>"
                return 1
                ;;
            *)
                command_to_exec="$*"
                break
                ;;
        esac
    done

    if [[ -z "$command_to_exec" ]]; then
        echo "❌ 実行するコマンドを指定してください"
        echo "使用法: gwt exec [-p|--parallel] [--dry-run] [-c|--continue] [--] <command>"
        echo ""
        echo "オプション:"
        echo "  -p, --parallel  各worktreeで並列実行"
        echo "  --dry-run       実際の実行は行わず、実行予定のコマンドのみ表示"
        echo "  -c, --continue  エラーが発生しても他のworktreeの処理を継続"
        echo ""
        echo "例:"
        echo "  gwt exec 'npm install'              # 各worktreeでnpm install"
        echo "  gwt exec --parallel 'npm test'      # 並列でテスト実行"
        echo "  gwt exec --dry-run 'git status'     # ドライラン"
        return 1
    fi

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    local current_worktree=$(git rev-parse --show-toplevel)
    local original_pwd=$(pwd)
    
    echo "🚀 全worktreeでコマンド実行..."
    echo "   コマンド: $command_to_exec"
    [[ "$parallel" == true ]] && echo "   モード: 並列実行" || echo "   モード: 順次実行"
    [[ "$dry_run" == true ]] && echo "   モード: ドライラン（実際の実行は行いません）"
    [[ "$continue_on_error" == true ]] && echo "   エラー処理: 継続" || echo "   エラー処理: 停止"
    echo ""

    local worktree_count=0
    local success_count=0
    local error_count=0
    local pids=()
    local temp_dir=""
    
    # 並列実行の場合は一時ディレクトリを作成
    if [[ "$parallel" == true ]]; then
        temp_dir=$(mktemp -d)
    fi
    
    # 各worktreeでコマンド実行
    git worktree list --porcelain | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            local branch_name=""
            local is_current=""
            
            # ブランチ名を取得（次の行を読む）
            read -r branch_line
            if [[ "$branch_line" =~ ^branch ]]; then
                branch_name="${branch_line#branch refs/heads/}"
            fi
            
            [[ "$worktree_path" == "$current_worktree" ]] && is_current=" 👈 現在"
            worktree_count=$((worktree_count + 1))
            
            echo "📂 $worktree_path$is_current"
            echo "   🌿 ブランチ: $branch_name"
            
            if [[ ! -d "$worktree_path" ]]; then
                echo "   ❌ ディレクトリが存在しません"
                error_count=$((error_count + 1))
                if [[ "$continue_on_error" == false ]]; then
                    break
                fi
                continue
            fi
            
            if [[ "$dry_run" == true ]]; then
                echo "   🔍 ドライラン: cd '$worktree_path' && $command_to_exec"
                success_count=$((success_count + 1))
            elif [[ "$parallel" == true ]]; then
                # 並列実行
                local output_file="$temp_dir/output_$worktree_count"
                (
                    cd "$worktree_path" 2>/dev/null
                    if [[ $? -eq 0 ]]; then
                        echo "   ▶️  実行中..." > "$output_file"
                        eval "$command_to_exec" >> "$output_file" 2>&1
                        echo "exit_code:$?" >> "$output_file"
                    else
                        echo "   ❌ ディレクトリへの移動に失敗" > "$output_file"
                        echo "exit_code:1" >> "$output_file"
                    fi
                ) &
                pids+=($!)
                echo "   ⏳ バックグラウンドで実行中（PID: $!）"
            else
                # 順次実行
                echo "   ▶️  実行中..."
                cd "$worktree_path" 2>/dev/null
                if [[ $? -eq 0 ]]; then
                    eval "$command_to_exec"
                    local exit_code=$?
                    if [[ $exit_code -eq 0 ]]; then
                        echo "   ✅ 成功"
                        success_count=$((success_count + 1))
                    else
                        echo "   ❌ 失敗 (終了コード: $exit_code)"
                        error_count=$((error_count + 1))
                        if [[ "$continue_on_error" == false ]]; then
                            cd "$original_pwd" 2>/dev/null
                            break
                        fi
                    fi
                else
                    echo "   ❌ ディレクトリへの移動に失敗"
                    error_count=$((error_count + 1))
                    if [[ "$continue_on_error" == false ]]; then
                        break
                    fi
                fi
                cd "$original_pwd" 2>/dev/null
            fi
            echo ""
        fi
    done
    
    # 並列実行の場合は全ての完了を待つ
    if [[ "$parallel" == true ]] && [[ "$dry_run" == false ]]; then
        echo "⏳ 全ての並列処理の完了を待機中..."
        echo ""
        
        for i in "${!pids[@]}"; do
            local pid=${pids[$i]}
            wait $pid
            
            local output_file="$temp_dir/output_$((i + 1))"
            if [[ -f "$output_file" ]]; then
                cat "$output_file" | head -n -1
                local exit_code=$(tail -n 1 "$output_file" | sed 's/exit_code://')
                if [[ "$exit_code" -eq 0 ]]; then
                    echo "   ✅ 成功"
                    success_count=$((success_count + 1))
                else
                    echo "   ❌ 失敗 (終了コード: $exit_code)"
                    error_count=$((error_count + 1))
                fi
                echo ""
            fi
        done
        
        # 一時ディレクトリのクリーンアップ
        rm -rf "$temp_dir"
    fi
    
    # 元のディレクトリに戻る
    cd "$original_pwd" 2>/dev/null
    
    # サマリー表示
    echo "📊 実行結果サマリー:"
    echo "   📂 処理したworktree数: $worktree_count"
    echo "   ✅ 成功数: $success_count"
    echo "   ❌ エラー数: $error_count"
    echo ""
    
    if [[ "$error_count" -gt 0 ]]; then
        echo "⚠️  エラーが発生したworktreeがあります。"
        return 1
    else
        echo "✅ 全ての実行が完了しました"
    fi
}

# 全worktree状態表示
function _gwt_status() {
    local compact=false
    local show_clean=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -c|--compact)
                compact=true
                shift
                ;;
            --show-clean)
                show_clean=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt status [-c|--compact] [--show-clean]"
                return 1
                ;;
            *)
                echo "❌ 不明な引数: $1"
                echo "使用法: gwt status [-c|--compact] [--show-clean]"
                return 1
                ;;
        esac
    done

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    local current_worktree=$(git rev-parse --show-toplevel)
    local original_pwd=$(pwd)
    
    echo "📊 Git Worktree状態一覧"
    [[ "$compact" == true ]] && echo "   表示モード: コンパクト" || echo "   表示モード: 詳細"
    echo ""

    local worktree_count=0
    local clean_count=0
    local dirty_count=0
    local ahead_count=0
    local behind_count=0
    
    # 各worktreeの状態を確認
    git worktree list --porcelain | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            local branch_name=""
            local is_current=""
            
            # ブランチ名を取得（次の行を読む）
            read -r branch_line
            if [[ "$branch_line" =~ ^branch ]]; then
                branch_name="${branch_line#branch refs/heads/}"
            fi
            
            [[ "$worktree_path" == "$current_worktree" ]] && is_current=" 👈 現在"
            worktree_count=$((worktree_count + 1))
            
            if [[ ! -d "$worktree_path" ]]; then
                echo "📂 $worktree_path$is_current"
                echo "   ❌ ディレクトリが存在しません"
                echo ""
                continue
            fi
            
            # worktreeディレクトリに移動して状態を取得
            cd "$worktree_path" 2>/dev/null
            if [[ $? -ne 0 ]]; then
                echo "📂 $worktree_path$is_current"
                echo "   ❌ ディレクトリへの移動に失敗"
                echo ""
                continue
            fi
            
            # Git status情報を取得
            local status_output=$(git status --porcelain 2>/dev/null)
            local is_clean=true
            local status_summary=""
            
            if [[ -n "$status_output" ]]; then
                is_clean=false
                dirty_count=$((dirty_count + 1))
                
                local modified_count=$(echo "$status_output" | grep -c "^ M" || echo "0")
                local added_count=$(echo "$status_output" | grep -c "^A" || echo "0")
                local deleted_count=$(echo "$status_output" | grep -c "^ D" || echo "0")
                local untracked_count=$(echo "$status_output" | grep -c "^??" || echo "0")
                
                local status_parts=()
                [[ "$modified_count" -gt 0 ]] && status_parts+=("M:$modified_count")
                [[ "$added_count" -gt 0 ]] && status_parts+=("A:$added_count")
                [[ "$deleted_count" -gt 0 ]] && status_parts+=("D:$deleted_count")
                [[ "$untracked_count" -gt 0 ]] && status_parts+=("?:$untracked_count")
                
                status_summary="📊 変更: $(IFS=', '; echo "${status_parts[*]}")"
            else
                clean_count=$((clean_count + 1))
                status_summary="✅ クリーン"
            fi
            
            # リモート同期状況
            local sync_status=""
            local ahead_behind=""
            if [[ -n "$branch_name" ]] && git rev-parse --verify "origin/$branch_name" >/dev/null 2>&1; then
                ahead_behind=$(git rev-list --left-right --count HEAD...origin/$branch_name 2>/dev/null)
                if [[ -n "$ahead_behind" ]]; then
                    local ahead=$(echo "$ahead_behind" | cut -f1)
                    local behind=$(echo "$ahead_behind" | cut -f2)
                    
                    if [[ "$ahead" -gt 0 ]] && [[ "$behind" -gt 0 ]]; then
                        sync_status="🔄 同期: +$ahead -$behind"
                        ahead_count=$((ahead_count + 1))
                        behind_count=$((behind_count + 1))
                    elif [[ "$ahead" -gt 0 ]]; then
                        sync_status="📤 未プッシュ: +$ahead"
                        ahead_count=$((ahead_count + 1))
                    elif [[ "$behind" -gt 0 ]]; then
                        sync_status="📥 未取得: -$behind"
                        behind_count=$((behind_count + 1))
                    else
                        sync_status="✅ 同期済み"
                    fi
                fi
            else
                sync_status="ℹ️  リモートなし"
            fi
            
            # 表示（クリーンな状態をスキップするかどうか）
            if [[ "$is_clean" == false ]] || [[ "$show_clean" == true ]] || [[ "$sync_status" != "✅ 同期済み" ]]; then
                echo "📂 $worktree_path$is_current"
                echo "   🌿 ブランチ: $branch_name"
                
                if [[ "$compact" == true ]]; then
                    # コンパクト表示
                    local compact_info="$status_summary"
                    if [[ "$sync_status" != "✅ 同期済み" ]]; then
                        compact_info="$compact_info | $sync_status"
                    fi
                    echo "   $compact_info"
                else
                    # 詳細表示
                    echo "   $status_summary"
                    echo "   $sync_status"
                    
                    # 最新コミット情報
                    local last_commit=$(git log -1 --format="%h %s" 2>/dev/null)
                    if [[ -n "$last_commit" ]]; then
                        echo "   🕒 最新: $last_commit"
                    fi
                fi
                echo ""
            fi
        fi
    done
    
    # 元のディレクトリに戻る
    cd "$original_pwd" 2>/dev/null
    
    # サマリー表示
    echo "📊 状態サマリー:"
    echo "   📂 Worktree総数: $worktree_count"
    echo "   ✅ クリーン: $clean_count"
    echo "   📊 変更あり: $dirty_count"
    echo "   📤 未プッシュ: $ahead_count"
    echo "   📥 未取得: $behind_count"
    echo ""
    
    if [[ "$dirty_count" -gt 0 ]] || [[ "$ahead_count" -gt 0 ]] || [[ "$behind_count" -gt 0 ]]; then
        echo "💡 推奨アクション:"
        [[ "$dirty_count" -gt 0 ]] && echo "   • 変更があるworktreeでコミットを作成"
        [[ "$ahead_count" -gt 0 ]] && echo "   • 未プッシュのコミットをpush"
        [[ "$behind_count" -gt 0 ]] && echo "   • リモートからの更新を取得（gwt sync）"
    else
        echo "✅ 全てのworktreeが最新でクリーンな状態です"
    fi
}

# 設定ファイル管理
function _gwt_load_config() {
    local config_file="${XDG_CONFIG_HOME:-$HOME/.config}/gwt/config.yml"
    
    # デフォルト設定
    GWT_CONFIG_EDITOR_DEFAULT="auto"
    GWT_CONFIG_EMACS_MODE="client"
    GWT_CONFIG_TMUX_ACTION="window"
    GWT_CONFIG_AUTO_DETECT="true"
    GWT_CONFIG_CLAUDE_INTEGRATION="true"
    
    # 設定ファイルが存在する場合は読み込み
    if [[ -f "$config_file" ]]; then
        # 簡易YAML読み込み（基本的なキー:値のみ対応）
        while IFS=': ' read -r key value; do
            # コメント行をスキップ
            [[ "$key" =~ ^[[:space:]]*# ]] && continue
            [[ -z "$key" ]] && continue
            
            # 値からクォートと空白を除去
            value=$(echo "$value" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//;s/^["'\'']*//;s/["'\'']*$//')
            
            case "$key" in
                "  default")
                    GWT_CONFIG_EDITOR_DEFAULT="$value"
                    ;;
                "    mode")
                    GWT_CONFIG_EMACS_MODE="$value"
                    ;;
                "  default_action")
                    GWT_CONFIG_TMUX_ACTION="$value"
                    ;;
                "  auto_detect")
                    GWT_CONFIG_AUTO_DETECT="$value"
                    ;;
                "  claude_code")
                    GWT_CONFIG_CLAUDE_INTEGRATION="$value"
                    ;;
            esac
        done < "$config_file"
    fi
}

# 設定ファイル初期化
function _gwt_init_config() {
    local config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/gwt"
    local config_file="$config_dir/config.yml"
    local dotfiles_example="$HOME/.dotfiles/.config/gwt/config.yml.example"
    
    # 設定ディレクトリを作成
    mkdir -p "$config_dir"
    
    # 設定ファイルが存在しない場合は作成
    if [[ ! -f "$config_file" ]]; then
        # dotfilesのサンプルファイルが存在する場合はそれを使用
        if [[ -f "$dotfiles_example" ]]; then
            cp "$dotfiles_example" "$config_file"
            echo "✅ dotfilesのサンプルから設定ファイルを作成しました: $config_file"
            echo "💡 設定を編集: gwt config edit"
        else
            # フォールバック: 基本設定をインライン生成
            cat > "$config_file" << 'EOF'
# Git Worktree管理ツール設定ファイル
# 詳細: gwt help

editor:
  default: "auto"               # auto, cursor, code, emacs
  auto_detect: true
  
  cursor:
    args: ["--new-window"]
    wait: false
  
  code:
    args: ["--new-window"]
    wait: false
  
  emacs:
    mode: "client"              # client, nw, gui
    server_timeout: 5
    fallback_to_direct: true

tmux:
  default_action: "window"      # window, split-h, split-v, session
  auto_detect: true
  naming_pattern: "gwt-{branch}"

integration:
  claude_code: true
  auto_cd: true

github:
  auto_push: true
  default_base: "main"
  open_browser: true

# 環境ファイル管理設定
environment:
  auto_setup: true              # worktree作成時に環境ファイルを自動セットアップ
  auto_port_assignment: true    # ポート番号の自動調整
  backup_existing: true         # 既存ファイルをバックアップ
  
  # 検出対象パターン（優先順位順）
  patterns:
    - ".env.development.example"
    - ".env.local.example"
    - ".env.staging.example"
    - ".env.test.example"
    - ".env.production.example"
    - ".env.example"
  
  # ポート設定
  port_range:
    start: 3000
    end: 9999
    increment: 100              # 初期増分値
  
  # 除外パターン
  exclude_patterns:
    - "node_modules/**"
    - ".git/**"
    - "dist/**"
    - "build/**"
EOF
            echo "✅ 基本設定ファイルを作成しました: $config_file"
            echo "💡 完全な設定例は dotfiles/.config/gwt/config.yml.example を参照"
        fi
    else
        echo "ℹ️  設定ファイルは既に存在します: $config_file"
    fi
}

# 利用可能エディタの検出
function _gwt_detect_editors() {
    local available_editors=()
    
    # 各エディタの存在確認
    command -v cursor >/dev/null 2>&1 && available_editors+=("cursor")
    command -v code >/dev/null 2>&1 && available_editors+=("code")
    command -v emacsclient >/dev/null 2>&1 && available_editors+=("emacsclient")
    command -v emacs >/dev/null 2>&1 && available_editors+=("emacs")
    command -v vim >/dev/null 2>&1 && available_editors+=("vim")
    command -v nvim >/dev/null 2>&1 && available_editors+=("nvim")
    
    echo "${available_editors[@]}"
}

# 最適なエディタを選択
function _gwt_select_editor() {
    local requested_editor="$1"
    local available_editors=($(\_gwt_detect_editors))
    
    # 明示的に指定された場合
    if [[ -n "$requested_editor" ]] && [[ "$requested_editor" != "auto" ]]; then
        for editor in "${available_editors[@]}"; do
            if [[ "$editor" == "$requested_editor" ]]; then
                echo "$requested_editor"
                return 0
            fi
        done
        echo "❌ エディタ '$requested_editor' が見つかりません"
        return 1
    fi
    
    # 設定ファイルのデフォルト
    if [[ "$GWT_CONFIG_EDITOR_DEFAULT" != "auto" ]]; then
        for editor in "${available_editors[@]}"; do
            if [[ "$editor" == "$GWT_CONFIG_EDITOR_DEFAULT" ]]; then
                echo "$GWT_CONFIG_EDITOR_DEFAULT"
                return 0
            fi
        done
    fi
    
    # 環境変数 $EDITOR
    if [[ -n "$EDITOR" ]]; then
        local editor_cmd=$(basename "$EDITOR")
        for editor in "${available_editors[@]}"; do
            if [[ "$editor" == "$editor_cmd" ]]; then
                echo "$editor_cmd"
                return 0
            fi
        done
    fi
    
    # 自動検出（優先順序）
    for preferred in cursor code emacsclient emacs vim nvim; do
        for editor in "${available_editors[@]}"; do
            if [[ "$editor" == "$preferred" ]]; then
                echo "$preferred"
                return 0
            fi
        done
    done
    
    echo "❌ 利用可能なエディタが見つかりません"
    return 1
}

# 環境検出
function _gwt_detect_environment() {
    local env_info=""
    
    # tmux環境
    [[ -n "$TMUX" ]] && env_info="${env_info}tmux "
    
    # SSH接続
    [[ -n "$SSH_CLIENT" ]] && env_info="${env_info}ssh "
    
    # GUIディスプレイ
    [[ -n "$DISPLAY" ]] && env_info="${env_info}gui "
    
    echo "${env_info% }"  # 末尾の空白を削除
}

# 設定コマンド
function _gwt_config() {
    local action="$1"
    
    case "$action" in
        "init")
            _gwt_init_config
            ;;
        "show"|"")
            local config_file="${XDG_CONFIG_HOME:-$HOME/.config}/gwt/config.yml"
            if [[ -f "$config_file" ]]; then
                echo "📋 現在の設定: $config_file"
                echo ""
                cat "$config_file"
            else
                echo "❌ 設定ファイルが見つかりません"
                echo "設定ファイルを作成: gwt config init"
            fi
            ;;
        "edit")
            local config_file="${XDG_CONFIG_HOME:-$HOME/.config}/gwt/config.yml"
            _gwt_init_config  # 設定ファイルがない場合は作成
            
            local selected_editor=$(_gwt_select_editor)
            if [[ $? -eq 0 ]]; then
                case "$selected_editor" in
                    "cursor")
                        cursor "$config_file"
                        ;;
                    "code")
                        code "$config_file"
                        ;;
                    "emacsclient")
                        emacsclient -nw "$config_file"
                        ;;
                    "emacs")
                        emacs -nw "$config_file"
                        ;;
                    *)
                        "${EDITOR:-vi}" "$config_file"
                        ;;
                esac
            fi
            ;;
        "path")
            echo "${XDG_CONFIG_HOME:-$HOME/.config}/gwt/config.yml"
            ;;
        *)
            echo "使用法: gwt config [init|show|edit|path]"
            echo ""
            echo "サブコマンド:"
            echo "  init  - 設定ファイルを初期化"
            echo "  show  - 現在の設定を表示"
            echo "  edit  - 設定ファイルを編集"
            echo "  path  - 設定ファイルのパスを表示"
            ;;
    esac
}

# エディタ起動
function _gwt_launch_editor() {
    local editor="$1"
    local worktree_path="$2"
    local emacs_mode="$3"
    local tmux_action="$4"
    
    # tmux環境での処理
    if [[ -n "$TMUX" ]] && [[ -n "$tmux_action" ]]; then
        local branch_name=$(basename "$worktree_path" | sed 's/.*-//')
        local tmux_name="gwt-$branch_name"
        
        case "$tmux_action" in
            "window")
                tmux new-window -c "$worktree_path" -n "$tmux_name"
                return $?
                ;;
            "split-h")
                tmux split-window -h -c "$worktree_path"
                return $?
                ;;
            "split-v")
                tmux split-window -v -c "$worktree_path"
                return $?
                ;;
            "session")
                tmux new-session -d -s "$tmux_name" -c "$worktree_path"
                return $?
                ;;
        esac
    fi
    
    # 各エディタの起動
    case "$editor" in
        "cursor")
            cursor --new-window "$worktree_path" &
            ;;
        "code")
            code --new-window "$worktree_path" &
            ;;
        "emacsclient")
            case "$emacs_mode" in
                "nw")
                    emacsclient -nw "$worktree_path"
                    ;;
                "gui")
                    emacsclient -c "$worktree_path" &
                    ;;
                "client"|*)
                    # Emacsサーバーが起動しているかチェック
                    if emacsclient -e "(server-running-p)" >/dev/null 2>&1; then
                        if [[ -n "$TMUX" ]] || [[ -n "$SSH_CLIENT" ]]; then
                            emacsclient -nw "$worktree_path"
                        else
                            emacsclient -c "$worktree_path" &
                        fi
                    else
                        echo "⚠️  Emacsサーバーが起動していません。直接Emacsを起動します..."
                        if [[ -n "$TMUX" ]] || [[ -n "$SSH_CLIENT" ]]; then
                            emacs -nw "$worktree_path"
                        else
                            emacs "$worktree_path" &
                        fi
                    fi
                    ;;
            esac
            ;;
        "emacs")
            case "$emacs_mode" in
                "nw")
                    emacs -nw "$worktree_path"
                    ;;
                *)
                    if [[ -n "$TMUX" ]] || [[ -n "$SSH_CLIENT" ]]; then
                        emacs -nw "$worktree_path"
                    else
                        emacs "$worktree_path" &
                    fi
                    ;;
            esac
            ;;
        "vim")
            vim "$worktree_path"
            ;;
        "nvim")
            nvim "$worktree_path"
            ;;
        *)
            echo "❌ 未対応のエディタ: $editor"
            return 1
            ;;
    esac
    
    return 0
}

# worktree選択して開く
function _gwt_open() {
    local editor=""
    local emacs_mode="$GWT_CONFIG_EMACS_MODE"
    local tmux_action=""
    local claude_mode=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --cursor)
                editor="cursor"
                shift
                ;;
            --code)
                editor="code"
                shift
                ;;
            --emacs)
                editor="emacs"
                shift
                ;;
            --emacs-nw)
                editor="emacs"
                emacs_mode="nw"
                shift
                ;;
            --emacs-client)
                editor="emacsclient"
                shift
                ;;
            --emacs-gui)
                editor="emacsclient"
                emacs_mode="gui"
                shift
                ;;
            --tmux-window)
                tmux_action="window"
                shift
                ;;
            --tmux-split-h)
                tmux_action="split-h"
                shift
                ;;
            --tmux-split-v)
                tmux_action="split-v"
                shift
                ;;
            --tmux-session)
                tmux_action="session"
                shift
                ;;
            --claude)
                claude_mode=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt open [editor-options] [tmux-options] [--claude]"
                return 1
                ;;
            *)
                echo "❌ 不明な引数: $1"
                echo "使用法: gwt open [editor-options] [tmux-options] [--claude]"
                return 1
                ;;
        esac
    done

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    # fzfが利用可能かチェック
    if ! command -v fzf >/dev/null 2>&1; then
        echo "❌ fzfが見つかりません。手動選択モードを使用してください"
        _gwt_open_manual "$editor" "$emacs_mode" "$tmux_action" "$claude_mode"
        return
    fi

    # fzfでworktree選択
    local current_worktree=$(git rev-parse --show-toplevel)
    local selected_worktree=$(git worktree list --porcelain | \
        awk '/^worktree/ {
            path = $2; 
            getline; 
            if (/^branch/) {
                branch = substr($0, 8);
                gsub(/refs\/heads\//, "", branch);
                if (path == "'$current_worktree'") {
                    printf "👈 %s (%s)\n", path, branch
                } else {
                    printf "%s (%s)\n", path, branch
                }
            }
        }' | \
        fzf --prompt="🔀 Worktreeを選択: " --height=40% --border --preview="echo {1} | xargs ls -la" | \
        awk '{print $1}' | \
        sed 's/👈 //')

    if [[ -z "$selected_worktree" ]]; then
        echo "❌ worktreeが選択されませんでした"
        return 1
    fi

    # エディタ選択
    if [[ -z "$editor" ]]; then
        editor=$(_gwt_select_editor)
        if [[ $? -ne 0 ]]; then
            return 1
        fi
    fi

    # tmuxアクション設定（デフォルト）
    if [[ -n "$TMUX" ]] && [[ -z "$tmux_action" ]] && [[ "$GWT_CONFIG_AUTO_DETECT" == "true" ]]; then
        tmux_action="$GWT_CONFIG_TMUX_ACTION"
    fi

    echo "🚀 Worktreeを開いています..."
    echo "   📂 パス: $selected_worktree"
    echo "   🖥️  エディタ: $editor"
    [[ -n "$tmux_action" ]] && echo "   📺 tmux: $tmux_action"
    
    # エディタ起動
    _gwt_launch_editor "$editor" "$selected_worktree" "$emacs_mode" "$tmux_action"
    
    if [[ $? -eq 0 ]]; then
        echo "✅ エディタ起動完了"
        
        # Claude Code統合
        if [[ "$claude_mode" == true ]] || [[ "$GWT_CONFIG_CLAUDE_INTEGRATION" == "true" && "$claude_mode" != false ]]; then
            echo ""
            echo "💡 Claude Codeを起動する場合:"
            echo "   cd \"$selected_worktree\" && claude"
        fi
    else
        echo "❌ エディタの起動に失敗しました"
        return 1
    fi
}

# 手動選択モード（fzf無し）
function _gwt_open_manual() {
    local editor="$1"
    local emacs_mode="$2"
    local tmux_action="$3"
    local claude_mode="$4"
    
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

    echo -n "🔀 開くworktreeの番号を入力 (1-${#worktrees[@]}): "
    read selection

    if [[ "$selection" =~ ^[0-9]+$ ]] && [[ "$selection" -ge 1 ]] && [[ "$selection" -le "${#worktrees[@]}" ]]; then
        local selected_worktree="${worktrees[$selection]}"
        
        # エディタ選択
        if [[ -z "$editor" ]]; then
            editor=$(_gwt_select_editor)
            if [[ $? -ne 0 ]]; then
                return 1
            fi
        fi
        
        # エディタ起動
        _gwt_launch_editor "$editor" "$selected_worktree" "$emacs_mode" "$tmux_action"
    else
        echo "❌ 無効な選択です"
        return 1
    fi
}

# GitHub PR作成
function _gwt_pr() {
    local base_branch=""
    local draft=false
    local no_push=false
    local no_browser=false
    local title=""
    local body=""
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -b|--base)
                base_branch="$2"
                shift 2
                ;;
            --draft)
                draft=true
                shift
                ;;
            --no-push)
                no_push=true
                shift
                ;;
            --no-browser)
                no_browser=true
                shift
                ;;
            -t|--title)
                title="$2"
                shift 2
                ;;
            --body)
                body="$2"
                shift 2
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt pr [-b <base>] [--draft] [--no-push] [--no-browser] [-t <title>] [--body <body>]"
                return 1
                ;;
            *)
                echo "❌ 不明な引数: $1"
                echo "使用法: gwt pr [-b <base>] [--draft] [--no-push] [--no-browser] [-t <title>] [--body <body>]"
                return 1
                ;;
        esac
    done

    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "❌ Gitリポジトリ内で実行してください"
        return 1
    fi

    # GitHub CLIの確認
    if ! command -v gh >/dev/null 2>&1; then
        echo "❌ GitHub CLI (gh) が見つかりません"
        echo "インストール: brew install gh"
        return 1
    fi

    # 認証確認
    if ! gh auth status >/dev/null 2>&1; then
        echo "❌ GitHub CLIでログインしていません"
        echo "ログイン: gh auth login"
        return 1
    fi

    # リモートがGitHubかどうか確認
    local remote_url=$(git remote get-url origin 2>/dev/null)
    if [[ ! "$remote_url" =~ github\.com ]]; then
        echo "❌ GitHubリポジトリではありません"
        echo "リモートURL: $remote_url"
        return 1
    fi

    # 現在のブランチ取得
    local current_branch=$(git symbolic-ref --short HEAD 2>/dev/null)
    if [[ -z "$current_branch" ]]; then
        echo "❌ 現在のブランチを取得できません"
        return 1
    fi

    # ベースブランチのデフォルト設定
    if [[ -z "$base_branch" ]]; then
        base_branch=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@' || echo "main")
    fi

    # mainブランチでのPR作成をチェック
    if [[ "$current_branch" == "$base_branch" ]]; then
        echo "❌ ベースブランチ ($base_branch) からはPRを作成できません"
        echo "機能ブランチを作成してください: gwt create feature/branch-name"
        return 1
    fi

    echo "🚀 GitHub PR作成を開始..."
    echo "   🌿 ブランチ: $current_branch"
    echo "   🎯 ベース: $base_branch"
    [[ "$draft" == true ]] && echo "   📝 タイプ: ドラフトPR"
    
    # 未コミットの変更をチェック
    local status_output=$(git status --porcelain)
    if [[ -n "$status_output" ]]; then
        echo "⚠️  未コミットの変更があります:"
        git status --short
        echo ""
        echo -n "コミットしてからPRを作成しますか？ (y/N): "
        read commit_confirm
        
        if [[ "$commit_confirm" =~ ^[yY]$ ]]; then
            echo -n "コミットメッセージを入力: "
            read commit_message
            if [[ -n "$commit_message" ]]; then
                git add .
                git commit -m "$commit_message"
                echo "✅ コミット完了"
            else
                echo "❌ コミットメッセージが空です"
                return 1
            fi
        else
            echo "❌ PR作成をキャンセルしました"
            return 1
        fi
    fi

    # プッシュ処理
    if [[ "$no_push" == false ]]; then
        echo "📤 ブランチをプッシュ中..."
        
        # リモートブランチが存在するかチェック
        if git rev-parse --verify "origin/$current_branch" >/dev/null 2>&1; then
            git push origin "$current_branch"
        else
            git push -u origin "$current_branch"
        fi
        
        if [[ $? -ne 0 ]]; then
            echo "❌ プッシュに失敗しました"
            return 1
        fi
        echo "✅ プッシュ完了"
    fi

    # PRタイトルとボディの自動生成
    if [[ -z "$title" ]]; then
        # ブランチ名からタイトルを生成
        title=$(echo "$current_branch" | sed 's/feature\///;s/fix\///;s/[-_]/ /g' | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) tolower(substr($i,2))}1')
    fi

    if [[ -z "$body" ]]; then
        # 最新コミットからボディを生成
        local latest_commits=$(git log --oneline "$base_branch..$current_branch" | head -5)
        if [[ -n "$latest_commits" ]]; then
            body="## 変更内容\n\n$(echo "$latest_commits" | sed 's/^/- /')\n\n## テスト\n\n- [ ] 動作確認済み\n- [ ] テスト追加済み"
        else
            body="## 変更内容\n\n<!-- 変更内容を記述してください -->\n\n## テスト\n\n- [ ] 動作確認済み\n- [ ] テスト追加済み"
        fi
    fi

    # PR作成
    echo "📋 PR作成中..."
    local pr_args=("--title" "$title" "--body" "$body" "--base" "$base_branch")
    
    [[ "$draft" == true ]] && pr_args+=("--draft")
    [[ "$no_browser" == true ]] && pr_args+=("--no-browser")

    local pr_url=$(gh pr create "${pr_args[@]}" 2>&1)
    local gh_exit_code=$?

    if [[ $gh_exit_code -eq 0 ]]; then
        echo "✅ PR作成完了!"
        echo "$pr_url" | grep -o 'https://github.com/[^[:space:]]*'
        
        # 統計情報
        local commit_count=$(git rev-list --count "$base_branch..$current_branch")
        local file_count=$(git diff --name-only "$base_branch..$current_branch" | wc -l)
        echo ""
        echo "📊 PR統計:"
        echo "   📝 コミット数: $commit_count"
        echo "   📄 変更ファイル数: $file_count"
        
    else
        echo "❌ PR作成に失敗しました"
        echo "$pr_url"
        return 1
    fi
}

# 環境ファイル管理システム
# 環境ファイルパターン検出
function _gwt_detect_env_files() {
    local search_dir="${1:-.}"
    local env_files=()
    
    # 検出パターンの優先順位
    local patterns=(
        ".env.development.example"
        ".env.local.example"
        ".env.staging.example"
        ".env.test.example"
        ".env.production.example"
        ".env.example"
    )
    
    # パターンごとにファイル検索
    for pattern in "${patterns[@]}"; do
        if [[ -f "$search_dir/$pattern" ]]; then
            local target_file=$(echo "$pattern" | sed 's/\.example$//')
            env_files+=("$pattern:$target_file")
        fi
    done
    
    echo "${env_files[@]}"
}

# 既存worktreeの環境ファイル検出
function _gwt_find_existing_env_files() {
    local current_worktree=$(git rev-parse --show-toplevel 2>/dev/null)
    local existing_envs=()
    
    # 全worktreeの環境ファイルを検索
    git worktree list --porcelain 2>/dev/null | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            
            # 現在のworktreeはスキップ
            [[ "$worktree_path" == "$current_worktree" ]] && continue
            
            # 各worktreeの環境ファイルを確認
            local env_patterns=(".env.development" ".env.local" ".env.staging" ".env.test" ".env")
            for env_file in "${env_patterns[@]}"; do
                if [[ -f "$worktree_path/$env_file" ]]; then
                    echo "$worktree_path:$env_file"
                fi
            done
        fi
    done
}

# ポート使用状況の分析
function _gwt_analyze_ports() {
    local used_ports=()
    local port_assignments=()
    
    # 既存worktreeのポート使用状況を収集
    git worktree list --porcelain 2>/dev/null | while IFS= read -r line; do
        if [[ "$line" =~ ^worktree ]]; then
            local worktree_path="${line#worktree }"
            local branch_name=""
            
            # ブランチ名を取得
            read -r branch_line
            if [[ "$branch_line" =~ ^branch ]]; then
                branch_name="${branch_line#branch refs/heads/}"
            fi
            
            # 環境ファイルからポート情報を抽出
            for env_file in "$worktree_path"/.env*; do
                [[ -f "$env_file" ]] || continue
                [[ "$env_file" =~ \.example$ ]] && continue
                
                while IFS='=' read -r key value; do
                    # コメント行をスキップ
                    [[ "$key" =~ ^[[:space:]]*# ]] && continue
                    [[ -z "$key" ]] && continue
                    
                    # ポート関連の変数を検出
                    if [[ "$key" =~ PORT$ ]] && [[ "$value" =~ ^[0-9]+$ ]]; then
                        echo "$value:$branch_name:$key"
                    fi
                done < "$env_file"
            done
        fi
    done
}

# 次の利用可能ポートを取得
function _gwt_get_next_port() {
    local base_port="${1:-3000}"
    local used_ports_raw=$(_gwt_analyze_ports)
    local used_ports=()
    
    # 使用中ポートの配列を作成
    while IFS= read -r port_info; do
        [[ -n "$port_info" ]] && used_ports+=($(echo "$port_info" | cut -d: -f1))
    done <<< "$used_ports_raw"
    
    # システムで使用中のポートもチェック
    local system_ports=$(ss -tuln 2>/dev/null | awk '/LISTEN/ {gsub(/.*:/, "", $5); gsub(/\].*/, "", $5); print $5}' | sort -n | uniq)
    used_ports+=($system_ports)
    
    # 重複を除去してソート
    used_ports=($(printf "%s\n" "${used_ports[@]}" | sort -n | uniq))
    
    # 次の利用可能ポートを検索
    local port=$base_port
    while true; do
        local is_used=false
        for used_port in "${used_ports[@]}"; do
            if [[ "$port" == "$used_port" ]]; then
                is_used=true
                break
            fi
        done
        
        if [[ "$is_used" == false ]]; then
            echo "$port"
            return 0
        fi
        
        ((port++))
    done
}

# 環境変数の値を置換
function _gwt_substitute_env_vars() {
    local input="$1"
    local worktree_path="$2"
    local branch_name="$3"
    
    # リポジトリ名を取得
    local repo_name=$(basename "$(git rev-parse --show-toplevel 2>/dev/null)")
    local branch_safe=$(echo "$branch_name" | sed 's/[^a-zA-Z0-9]/_/g')
    
    # 変数置換
    local result="$input"
    result=$(echo "$result" | sed "s/{{repo}}/$repo_name/g")
    result=$(echo "$result" | sed "s/{{branch}}/$branch_name/g")
    result=$(echo "$result" | sed "s/{{branch_safe}}/$branch_safe/g")
    result=$(echo "$result" | sed "s/{{user}}/$(whoami)/g")
    result=$(echo "$result" | sed "s/{{timestamp}}/$(date +%s)/g")
    
    # ポート関連の置換
    while [[ "$result" =~ \{\{auto_port([+\-]?[0-9]*)\}\} ]]; do
        local offset="${BASH_REMATCH[1]:-0}"
        local base_port=$(_gwt_get_next_port 3000)
        local final_port=$((base_port + offset))
        result=$(echo "$result" | sed "s/{{auto_port${offset}}}/$final_port/")
    done
    
    # ランダム文字列の生成
    while [[ "$result" =~ \{\{random([0-9]*)\}\} ]]; do
        local length="${BASH_REMATCH[1]:-32}"
        local random_str=$(openssl rand -hex $((length / 2)) 2>/dev/null || head -c $((length / 2)) /dev/urandom | xxd -p)
        result=$(echo "$result" | sed "s/{{random${length}}}/$random_str/")
    done
    
    echo "$result"
}

# 環境ファイルの設定値を抽出
function _gwt_parse_env_file() {
    local env_file="$1"
    local parsed_vars=()
    
    [[ ! -f "$env_file" ]] && return 1
    
    while IFS='=' read -r key value; do
        # コメント行と空行をスキップ
        [[ "$key" =~ ^[[:space:]]*# ]] && continue
        [[ -z "$key" ]] && continue
        
        # 値からクォートを除去
        value=$(echo "$value" | sed 's/^["'\'']*//;s/["'\'']*$//')
        
        echo "$key=$value"
    done < "$env_file"
}

# 環境設定の類似度計算
function _gwt_calculate_similarity() {
    local source_env="$1"
    local target_env="$2"
    
    local source_vars=()
    local target_vars=()
    local common_count=0
    local total_count=0
    
    # 環境変数を配列に読み込み
    while IFS= read -r line; do
        [[ -n "$line" ]] && source_vars+=("$line")
    done < <(_gwt_parse_env_file "$source_env")
    
    while IFS= read -r line; do
        [[ -n "$line" ]] && target_vars+=("$line")
    done < <(_gwt_parse_env_file "$target_env")
    
    # 共通の変数をカウント
    for source_var in "${source_vars[@]}"; do
        local source_key=$(echo "$source_var" | cut -d= -f1)
        total_count=$((total_count + 1))
        
        for target_var in "${target_vars[@]}"; do
            local target_key=$(echo "$target_var" | cut -d= -f1)
            if [[ "$source_key" == "$target_key" ]]; then
                common_count=$((common_count + 1))
                break
            fi
        done
    done
    
    # 類似度を計算（0-100%）
    if [[ $total_count -gt 0 ]]; then
        echo $((common_count * 100 / total_count))
    else
        echo "0"
    fi
}

# ヘルプ表示
function _gwt_help() {
    cat << 'EOF'
🚀 Git Worktree管理コマンド (Claude Code対応版)

📋 使用法:
  gwt create [-s] <branch-name> [base-branch]  新しいworktreeを作成
  gwt list [-v]                                worktree一覧を表示
  gwt switch                                   worktree切り替え (fzf対応)
  gwt remove [-d] [--force] <worktree-name>    worktreeを削除
  gwt sync [--dry-run] [--rebase]              全worktreeを同期
  gwt exec [-p] [--dry-run] [-c] <command>     全worktreeでコマンド実行
  gwt status [-c] [--show-clean]               全worktree状態表示
  gwt open [editor-options] [tmux-options]     エディタでworktreeを開く
  gwt pr [-b <base>] [--draft] [--no-push]     GitHub PR作成
  gwt config [init|show|edit|path]             設定ファイル管理
  gwt env [detect|analyze|setup] [options]     環境ファイル管理
  gwt clean                                    メンテナンス・クリーンアップ
  gwt help                                     このヘルプを表示

🚩 オプション:
  create:
    -s, --switch    worktree作成後に自動的にそのディレクトリに移動
  list:
    -v, --verbose   詳細情報を表示（サイズ、変更状況、同期状態など）
  remove:
    -d, --delete-branch  対応するブランチも一緒に削除
    --force              未コミット変更があっても強制削除
  sync:
    --dry-run       実際の変更は行わず、実行予定の操作のみ表示
    --rebase        マージの代わりにリベースで同期
  exec:
    -p, --parallel  各worktreeで並列実行
    --dry-run       実際の実行は行わず、実行予定のコマンドのみ表示
    -c, --continue  エラーが発生しても他のworktreeの処理を継続
  status:
    -c, --compact   コンパクト表示（1行で状態を表示）
    --show-clean    クリーンなworktreeも表示
  open:
    --cursor        Cursorで開く
    --code          VS Codeで開く
    --emacs         Emacsで開く
    --emacs-nw      Emacs（ターミナル）で開く
    --emacs-client  emacsclientで開く
    --tmux-window   tmux新ウィンドウで開く
    --tmux-split-h  tmux水平分割で開く
    --tmux-split-v  tmux垂直分割で開く
    --claude        Claude Code統合モード
  pr:
    -b, --base      ベースブランチを指定（デフォルト：main）
    --draft         ドラフトPRとして作成
    --no-push       プッシュせずにPR作成のみ
    --no-browser    ブラウザを開かない
    -t, --title     PRタイトルを指定
    --body          PR本文を指定

🔧 短縮エイリアス:
  gwt c    ≡ gwt create
  gwt l    ≡ gwt list
  gwt s    ≡ gwt switch
  gwt o    ≡ gwt open
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

# 環境ファイル管理
function _gwt_env() {
    local command="$1"
    shift
    
    case "$command" in
        "detect"|"d")
            _gwt_env_detect "$@"
            ;;
        "analyze"|"a")
            _gwt_env_analyze "$@"
            ;;
        "setup"|"s")
            _gwt_env_setup "$@"
            ;;
        "help"|"h"|"")
            _gwt_env_help
            ;;
        *)
            echo "❌ 不明なサブコマンド: $command"
            _gwt_env_help
            return 1
            ;;
    esac
}

# 環境ファイル検出
function _gwt_env_detect() {
    local search_dir="${1:-.}"
    local show_content=false
    local recursive=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -c|--content)
                show_content=true
                shift
                ;;
            -r|--recursive)
                recursive=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt env detect [-c|--content] [-r|--recursive] [directory]"
                return 1
                ;;
            *)
                search_dir="$1"
                shift
                ;;
        esac
    done
    
    if [[ ! -d "$search_dir" ]]; then
        echo "❌ ディレクトリが見つかりません: $search_dir"
        return 1
    fi
    
    echo "🔍 環境ファイル検出結果: $search_dir"
    echo ""
    
    # 検出された環境ファイルを格納する配列
    local -a env_files
    local -a example_files
    
    # 環境ファイルパターンの検出
    local detected_files=($(_gwt_detect_env_files "$search_dir"))
    
    for file in "${detected_files[@]}"; do
        if [[ -f "$file" ]]; then
            if [[ "$file" == *.example ]]; then
                example_files+=("$file")
            else
                env_files+=("$file")
            fi
        fi
    done
    
    # 実際の環境ファイルを表示
    if [[ ${#env_files[@]} -gt 0 ]]; then
        echo "📄 現在の環境ファイル:"
        for file in "${env_files[@]}"; do
            local size=$(stat -f%z "$file" 2>/dev/null || echo "不明")
            local modified=$(stat -f%Sm "$file" 2>/dev/null || echo "不明")
            echo "   ✅ $(basename "$file") (${size}B, 更新: $modified)"
            
            if [[ "$show_content" == true ]]; then
                echo "      内容プレビュー:"
                # ポート関連の設定を特に注目
                grep -E "PORT|port|Port" "$file" 2>/dev/null | head -3 | sed 's/^/      /'
                echo ""
            fi
        done
        echo ""
    fi
    
    # テンプレートファイルを表示
    if [[ ${#example_files[@]} -gt 0 ]]; then
        echo "📋 テンプレートファイル:"
        for file in "${example_files[@]}"; do
            local size=$(stat -f%z "$file" 2>/dev/null || echo "不明")
            echo "   📄 $(basename "$file") (${size}B)"
            
            if [[ "$show_content" == true ]]; then
                echo "      ポート設定のプレビュー:"
                grep -E "PORT|port|Port" "$file" 2>/dev/null | head -3 | sed 's/^/      /'
                echo ""
            fi
        done
        echo ""
    fi
    
    # 検出されなかった場合
    if [[ ${#env_files[@]} -eq 0 ]] && [[ ${#example_files[@]} -eq 0 ]]; then
        echo "ℹ️  環境ファイルが見つかりませんでした"
        echo ""
        echo "💡 一般的な環境ファイル名:"
        echo "   • .env"
        echo "   • .env.local"
        echo "   • .env.development"
        echo "   • .env.example"
        echo ""
    fi
    
    # 再帰検索
    if [[ "$recursive" == true ]]; then
        echo "🔄 サブディレクトリの検索..."
        for subdir in "$search_dir"/*; do
            if [[ -d "$subdir" ]] && [[ "$(basename "$subdir")" != "." ]] && [[ "$(basename "$subdir")" != ".." ]]; then
                local sub_files=($(_gwt_detect_env_files "$subdir"))
                if [[ ${#sub_files[@]} -gt 0 ]]; then
                    echo "   📂 $(basename "$subdir"):"
                    for file in "${sub_files[@]}"; do
                        if [[ -f "$file" ]]; then
                            echo "      📄 $(basename "$file")"
                        fi
                    done
                fi
            fi
        done
    fi
}

# 環境設定分析
function _gwt_env_analyze() {
    local target_dir="${1:-.}"
    local compare_all=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --all)
                compare_all=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt env analyze [--all] [directory]"
                return 1
                ;;
            *)
                target_dir="$1"
                shift
                ;;
        esac
    done
    
    if [[ ! -d "$target_dir" ]]; then
        echo "❌ ディレクトリが見つかりません: $target_dir"
        return 1
    fi
    
    echo "📊 環境設定分析: $target_dir"
    echo ""
    
    # 現在のディレクトリの環境ファイル
    local current_env_files=($(_gwt_detect_env_files "$target_dir"))
    
    if [[ ${#current_env_files[@]} -eq 0 ]]; then
        echo "ℹ️  分析対象の環境ファイルが見つかりません"
        return 1
    fi
    
    # ポート使用状況の分析
    echo "🔌 ポート使用状況:"
    local -a used_ports
    
    for file in "${current_env_files[@]}"; do
        if [[ -f "$file" ]]; then
            echo "   📄 $(basename "$file"):"
            # ポート番号を抽出
            local ports=$(grep -E "PORT|port|Port" "$file" 2>/dev/null | grep -oE '[0-9]{4,5}' | sort -u)
            for port in $ports; do
                echo "      🔌 ポート $port"
                used_ports+=("$port")
            done
            
            if [[ -z "$ports" ]]; then
                echo "      ℹ️  ポート設定なし"
            fi
        fi
    done
    echo ""
    
    # 他のworktreeとの比較（compare_allオプション）
    if [[ "$compare_all" == true ]] && git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "🔄 他のworktreeとの比較:"
        
        local current_worktree=$(git rev-parse --show-toplevel)
        git worktree list --porcelain | grep '^worktree' | awk '{print $2}' | while read worktree_path; do
            if [[ "$worktree_path" != "$current_worktree" ]]; then
                local other_env_files=($(_gwt_detect_env_files "$worktree_path"))
                if [[ ${#other_env_files[@]} -gt 0 ]]; then
                    echo "   📂 $(basename "$worktree_path"):"
                    for file in "${other_env_files[@]}"; do
                        if [[ -f "$file" ]]; then
                            local other_ports=$(grep -E "PORT|port|Port" "$file" 2>/dev/null | grep -oE '[0-9]{4,5}' | sort -u)
                            for port in $other_ports; do
                                if printf '%s\n' "${used_ports[@]}" | grep -q "^$port$"; then
                                    echo "      ⚠️  ポート $port (競合)"
                                else
                                    echo "      🔌 ポート $port"
                                fi
                            done
                        fi
                    done
                fi
            fi
        done
        echo ""
    fi
    
    # 次に利用可能なポート番号を提案
    if [[ ${#used_ports[@]} -gt 0 ]]; then
        echo "💡 推奨ポート番号:"
        local base_port=3000
        local suggested_ports=()
        
        for ((i=0; i<5; i++)); do
            local candidate=$((base_port + i * 1000))
            while printf '%s\n' "${used_ports[@]}" | grep -q "^$candidate$"; do
                candidate=$((candidate + 1))
            done
            suggested_ports+=("$candidate")
        done
        
        for port in "${suggested_ports[@]}"; do
            echo "   🆓 ポート $port"
        done
        echo ""
    fi
}

# 環境ファイルのセットアップ
function _gwt_env_setup() {
    local template_source=""
    local target_dir="${PWD}"
    local auto_port=false
    local dry_run=false
    local force=false
    
    # オプション解析
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --template)
                template_source="$2"
                shift 2
                ;;
            --target)
                target_dir="$2"
                shift 2
                ;;
            --auto-port)
                auto_port=true
                shift
                ;;
            --dry-run)
                dry_run=true
                shift
                ;;
            --force)
                force=true
                shift
                ;;
            -*)
                echo "❌ 不明なオプション: $1"
                echo "使用法: gwt env setup [--template source] [--target dir] [--auto-port] [--dry-run] [--force]"
                return 1
                ;;
            *)
                if [[ -z "$template_source" ]]; then
                    template_source="$1"
                else
                    target_dir="$1"
                fi
                shift
                ;;
        esac
    done
    
    # テンプレートファイルの自動検出
    if [[ -z "$template_source" ]]; then
        echo "🔍 テンプレートファイルを自動検出中..."
        
        # Git worktree内でテンプレートを検索
        if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
            local repo_root=$(git rev-parse --show-toplevel)
            local template_files=($(_gwt_detect_env_files "$repo_root" | grep '\.example$'))
            
            if [[ ${#template_files[@]} -gt 0 ]]; then
                template_source="${template_files[0]}"
                echo "   ✅ テンプレート発見: $(basename "$template_source")"
            fi
        fi
        
        if [[ -z "$template_source" ]]; then
            echo "❌ テンプレートファイルが見つかりません"
            echo "💡 手動でテンプレートファイルを指定してください: gwt env setup --template /path/to/.env.example"
            return 1
        fi
    fi
    
    if [[ ! -f "$template_source" ]]; then
        echo "❌ テンプレートファイルが見つかりません: $template_source"
        return 1
    fi
    
    if [[ ! -d "$target_dir" ]]; then
        echo "❌ ターゲットディレクトリが見つかりません: $target_dir"
        return 1
    fi
    
    echo "🛠️  環境ファイルセットアップ"
    echo "   📋 テンプレート: $(basename "$template_source")"
    echo "   📂 ターゲット: $target_dir"
    echo ""
    
    # ターゲットファイル名を決定
    local template_basename=$(basename "$template_source")
    local target_filename=""
    
    if [[ "$template_basename" == *.example ]]; then
        target_filename="${template_basename%.example}"
    else
        target_filename=".env"
    fi
    
    local target_file="$target_dir/$target_filename"
    
    # 既存ファイルの確認
    if [[ -f "$target_file" ]] && [[ "$force" == false ]]; then
        echo "⚠️  既存の環境ファイルが存在します: $(basename "$target_file")"
        echo "💡 上書きする場合は --force オプションを使用してください"
        return 1
    fi
    
    # ドライランの場合
    if [[ "$dry_run" == true ]]; then
        echo "🔍 ドライラン実行中..."
        echo "   📋 コピー元: $template_source"
        echo "   📄 コピー先: $target_file"
        
        if [[ "$auto_port" == true ]]; then
            echo "   🔌 ポート自動設定が有効"
            # 使用中のポートを表示
            local used_ports=($(_gwt_collect_used_ports))
            if [[ ${#used_ports[@]} -gt 0 ]]; then
                echo "   📊 現在使用中のポート: ${used_ports[*]}"
            fi
        fi
        
        echo "   ✅ 実際の変更は行われません"
        return 0
    fi
    
    # ファイルのコピーと設定
    echo "📋 環境ファイルを作成中..."
    
    # テンプレートをコピー
    cp "$template_source" "$target_file"
    
    if [[ $? -ne 0 ]]; then
        echo "❌ ファイルのコピーに失敗しました"
        return 1
    fi
    
    echo "   ✅ ファイルコピー完了: $(basename "$target_file")"
    
    # ポート自動設定
    if [[ "$auto_port" == true ]]; then
        echo "🔌 ポート番号を自動設定中..."
        
        # 使用中のポートを収集
        local used_ports=($(_gwt_collect_used_ports))
        
        # ポート設定行を検索・置換
        local port_lines=$(grep -n "PORT" "$target_file" 2>/dev/null)
        
        if [[ -n "$port_lines" ]]; then
            while IFS= read -r line; do
                local line_num=$(echo "$line" | cut -d: -f1)
                local line_content=$(echo "$line" | cut -d: -f2-)
                
                # 現在のポート番号を抽出
                local current_port=$(echo "$line_content" | grep -oE '[0-9]{4,5}' | head -1)
                
                if [[ -n "$current_port" ]]; then
                    # 新しいポート番号を生成
                    local new_port=$(_gwt_generate_available_port "$current_port" "${used_ports[@]}")
                    
                    # ファイル内のポート番号を置換
                    sed -i.bak "${line_num}s/$current_port/$new_port/g" "$target_file"
                    
                    echo "   🔄 ポート更新: $current_port → $new_port"
                    used_ports+=("$new_port")
                fi
            done <<< "$port_lines"
            
            # バックアップファイルを削除
            rm -f "$target_file.bak"
        else
            echo "   ℹ️  ポート設定が見つかりませんでした"
        fi
    fi
    
    echo ""
    echo "✅ 環境ファイルセットアップ完了"
    echo "   📄 作成されたファイル: $target_file"
    echo "   💡 必要に応じて設定値を調整してください"
}

# 環境管理ヘルプ
function _gwt_env_help() {
    cat <<'EOF'

🌍 gwt env - 環境ファイル管理システム

📋 使用法:
  gwt env detect [-c] [-r] [directory]     環境ファイルを検出・表示
  gwt env analyze [--all] [directory]      環境設定を分析（ポート競合等）
  gwt env setup [options] [template]       環境ファイルをセットアップ

🔍 detect オプション:
  -c, --content      ファイルの内容プレビューを表示
  -r, --recursive    サブディレクトリも再帰的に検索

📊 analyze オプション:
  --all             他のworktreeとも比較分析

🛠️ setup オプション:
  --template        テンプレートファイルを指定
  --target          ターゲットディレクトリを指定
  --auto-port       ポート番号を自動調整
  --dry-run         実際の変更を行わずプレビュー
  --force           既存ファイルを上書き

💡 使用例:
  # 環境ファイルの検出
  gwt env detect -c

  # ポート競合の分析
  gwt env analyze --all

  # 環境ファイルの自動セットアップ
  gwt env setup --auto-port

  # 特定テンプレートからのセットアップ
  gwt env setup --template .env.development.example --auto-port

🎯 機能:
  ✅ 複数の環境ファイル形式を自動検出
  ✅ worktree間のポート競合を自動回避
  ✅ 既存設定の継承とマージ
  ✅ インテリジェントなポート番号提案
  ✅ ドライラン機能で安全な設定確認

EOF
}

# 使用中のポートを収集するユーティリティ関数
function _gwt_collect_used_ports() {
    local -a used_ports
    
    # 現在のworktreeのポートを収集
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        local current_worktree=$(git rev-parse --show-toplevel)
        
        # 全worktreeのポートを収集
        git worktree list --porcelain | grep '^worktree' | awk '{print $2}' | while read worktree_path; do
            local env_files=($(_gwt_detect_env_files "$worktree_path"))
            for file in "${env_files[@]}"; do
                if [[ -f "$file" ]]; then
                    local ports=$(grep -E "PORT|port|Port" "$file" 2>/dev/null | grep -oE '[0-9]{4,5}' | sort -u)
                    for port in $ports; do
                        echo "$port"
                    done
                fi
            done
        done | sort -u
    fi
    
    # システムで使用中のポートもチェック（オプション）
    # netstat や lsof でアクティブなポートも確認可能
}

# 利用可能なポート番号を生成するユーティリティ関数
function _gwt_generate_available_port() {
    local base_port="$1"
    shift
    local used_ports=("$@")
    
    # ベースポートが指定されていない場合のデフォルト
    if [[ -z "$base_port" ]] || [[ ! "$base_port" =~ ^[0-9]+$ ]]; then
        base_port=3000
    fi
    
    # ベースポートの範囲に基づいて適切な増分を決定
    local increment=1
    if [[ "$base_port" -ge 8000 ]]; then
        increment=1
    elif [[ "$base_port" -ge 5000 ]]; then
        increment=10
    else
        increment=100
    fi
    
    local candidate="$base_port"
    
    # 使用中のポートをチェックして利用可能なポートを見つける
    while printf '%s\n' "${used_ports[@]}" | grep -q "^$candidate$"; do
        candidate=$((candidate + increment))
        
        # 無限ループ防止（65535がポート番号の上限）
        if [[ "$candidate" -gt 65535 ]]; then
            # 範囲を狭めて再試行
            candidate=$((base_port + 1))
            increment=1
            # それでも見つからない場合は諦める
            if [[ "$candidate" -gt $((base_port + 1000)) ]]; then
                candidate=$((base_port + $(($RANDOM % 1000))))
                break
            fi
        fi
    done
    
    echo "$candidate"
}

# エイリアス競合の最終解決: 読み込み後に強制的にエイリアスを削除（Oh My Zsh git plugin対応）
unalias gwt gwta gwtls gwtmv gwtrm 2>/dev/null || true

# カスタムエイリアス定義（aliases.zshから移動）
alias gw='gwt'
alias gwc='gwt create'
alias gwl='gwt list'
alias gws='gwt switch'
alias gwr='gwt remove'
