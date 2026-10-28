<#
.SYNOPSIS
Publishes the committed LISP files to the team's shared (SharePoint-synced) folder.

.DESCRIPTION
Copies every .lsp file tracked by this repo into the folder the team's AutoCAD
loads from. Only work that is committed on main and pushed to GitHub gets
published, so the shared copy always matches a commit there.

- Files are copied only when their content differs.
- .lsp files in the target that are no longer in the repo are listed, not deleted.
- PUBLISHED.txt in the target records which commit is live.

The target path stays out of the repo, since the repo is public. Pass -Target
with -SaveTarget once; later runs read it from .publish-target (gitignored).
Don't end a quoted -Target path with a backslash.

.EXAMPLE
.\publish.cmd -Target "C:\Users\me\Org\CAD Team - Documents\LISP" -SaveTarget
Publish to that folder and remember it for next time.

.EXAMPLE
.\publish.cmd -WhatIf
Show what would be copied without changing anything.
#>
[CmdletBinding(SupportsShouldProcess = $true)]
param(
    [string]$Target,
    [switch]$SaveTarget
)

$ErrorActionPreference = 'Stop'
$repo = $PSScriptRoot
$targetFile = Join-Path $repo '.publish-target'

function Stop-Publish([string]$Message) {
    Write-Host "PUBLISH STOPPED: $Message" -ForegroundColor Red
    exit 1
}

function Invoke-Git {
    $out = & git -C $repo @args
    if ($LASTEXITCODE -ne 0) { Stop-Publish "git $($args -join ' ') failed." }
    $out
}

# ---------- Where to publish ----------

if (-not $Target) {
    if (Test-Path -LiteralPath $targetFile) {
        $Target = (Get-Content -LiteralPath $targetFile -Raw).Trim()
    } else {
        Stop-Publish "No target folder. Run once with -Target <folder> -SaveTarget."
    }
}
$Target = $Target.TrimEnd('\')
if (-not (Test-Path -LiteralPath $Target -PathType Container)) {
    Stop-Publish "Target folder not found: $Target (is the SharePoint library synced on this PC?)"
}

# ---------- Only committed, pushed work on main ----------

$branch = Invoke-Git rev-parse --abbrev-ref HEAD
if ($branch -ne 'main') { Stop-Publish "On branch '$branch'. Switch to main first." }

$dirty = Invoke-Git status --porcelain
if ($dirty) {
    Stop-Publish ("Uncommitted changes. Commit or discard them first:`n" + ($dirty -join "`n"))
}

Invoke-Git fetch --quiet origin | Out-Null
$ahead, $behind = (Invoke-Git rev-list --left-right --count 'HEAD...@{u}') -split '\s+'
if ([int]$ahead -gt 0) { Stop-Publish "$ahead commit(s) not on GitHub yet. Run git push first." }
if ([int]$behind -gt 0) { Stop-Publish "GitHub has $behind newer commit(s). Run git pull first." }

$short, $full, $subject = (Invoke-Git log -1 '--format=%h%x09%H%x09%s') -split "`t", 3

# ---------- Copy changed files ----------

$files = @(Invoke-Git ls-files -- '*.lsp')
$added = 0; $updated = 0; $same = 0
foreach ($rel in $files) {
    $src = Join-Path $repo $rel
    $dst = Join-Path $Target $rel
    if (Test-Path -LiteralPath $dst) {
        if ((Get-FileHash -LiteralPath $src).Hash -eq (Get-FileHash -LiteralPath $dst).Hash) {
            $same++
            continue
        }
        $action = 'Update'
    } else {
        $action = 'Add'
    }
    if ($PSCmdlet.ShouldProcess($dst, $action)) {
        New-Item -ItemType Directory -Force -Path (Split-Path $dst) | Out-Null
        Copy-Item -LiteralPath $src -Destination $dst -Force
        Write-Host "  $action  $rel"
    }
    if ($action -eq 'Add') { $added++ } else { $updated++ }
}

# ---------- Record the live commit ----------

$remote = Invoke-Git remote get-url origin
if ($remote -match '^https://github\.com/(.+?)(\.git)?$') {
    $source = "https://github.com/$($Matches[1])/commit/$full"
} else {
    $source = "$remote @ $full"
}
$stamp = @(
    "Commit:    $short  $subject"
    "Source:    $source"
    "Published: $(Get-Date -Format 'yyyy-MM-dd HH:mm') by $env:USERNAME"
)
$stampFile = Join-Path $Target 'PUBLISHED.txt'
$live = if (Test-Path -LiteralPath $stampFile) { Get-Content -LiteralPath $stampFile -TotalCount 1 } else { '' }
if ($live -ne $stamp[0] -and $PSCmdlet.ShouldProcess($stampFile, 'Write')) {
    Set-Content -LiteralPath $stampFile -Value $stamp -Encoding UTF8
}

if ($SaveTarget -and $PSCmdlet.ShouldProcess($targetFile, "Save target $Target")) {
    Set-Content -LiteralPath $targetFile -Value $Target -Encoding UTF8
}

# ---------- Report ----------

$verb = if ($WhatIfPreference) { 'Would publish' } else { 'Published' }
Write-Host "$verb $short to ${Target}: $added added, $updated updated, $same unchanged." -ForegroundColor Green

$stale = @(Get-ChildItem -LiteralPath $Target -Recurse -File -Filter *.lsp |
    ForEach-Object { $_.FullName.Substring($Target.Length + 1) } |
    Where-Object { ($files -replace '/', '\') -notcontains $_ })
if ($stale) {
    Write-Host "Not in the repo, left in place (delete by hand if no longer needed):" -ForegroundColor Yellow
    $stale | ForEach-Object { Write-Host "  $_" }
}
