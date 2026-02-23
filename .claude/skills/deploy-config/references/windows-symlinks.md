# Windows Symlinks in MSYS2/MINGW64

## OSTYPE の罠

同じ MSYS2 環境でもシェルにより `$OSTYPE` が異なる:

| シェル | `$OSTYPE` | 備考 |
|--------|-----------|------|
| bash (MINGW64) | `msys` | MSYS2 デフォルト |
| zsh (MINGW64) | `cygwin` | zsh は Cygwin 互換レイヤを報告 |

`link.sh` は zsh スクリプトのため、`case "${OSTYPE}" in msys|cygwin)` で両方をマッチさせる。

## ln -s の deepcopy 問題

MSYS2 の `ln -s` はデフォルトで **ファイルをコピー** する（シンボリックリンクを作成しない）。
`MSYS=winsymlinks:nativestrict` を設定すれば Windows ネイティブ symlink を作成できるが、
**Developer Mode または管理者権限** が必要。

回避策: `cmd //c mklink` を直接使用する。

## mklink の使い方

| コマンド | 対象 | 動作 |
|---------|------|------|
| `mklink <link> <target>` | ファイル | シンボリックリンク（Developer Mode 必要） |
| `mklink /D <link> <target>` | ディレクトリ | ディレクトリ symlink（Developer Mode 必要） |
| `mklink /H <link> <target>` | ファイル | ハードリンク（権限不要、同一ドライブ限定） |
| `mklink /J <link> <target>` | ディレクトリ | ジャンクション（権限不要、ローカルのみ） |

`link.sh` ではファイルに `mklink`、ディレクトリに `mklink /D` を使用。

## cmd //c の理由

MSYS2 環境から `cmd` を呼ぶ際、`/c` は `/` で始まるため Unix パスとして解釈される。
`//c` とスラッシュを二重にすることで、MSYS2 のパス変換を回避し `cmd /c` として渡す。

```bash
# NG: /c が C:\ に変換される可能性
cmd /c "mklink ..."

# OK: //c は /c として cmd.exe に渡される
cmd //c "mklink ..."
```

## cygpath によるパス変換

XDG_CONFIG_HOME 配下など絶対パスが必要な場合、`cygpath -w` で Windows パスに変換する:

```bash
cmd //c "mklink /D $(cygpath -w "$dst") $(cygpath -w "$src")"
```

ホーム直下のファイル/ディレクトリは相対パスで対応できるため `cygpath` 不要。

## Developer Mode の権限要件

Windows 10 (1703+) では Developer Mode を有効にすると、管理者権限なしで symlink を作成できる:

```
設定 > 更新とセキュリティ > 開発者向け > 開発者モード
```

Developer Mode が無効な場合、`mklink` は管理者権限（UAC 昇格）が必要。
`validate.sh` でレジストリキーをチェックし、未検出時は WARN を出力する。
