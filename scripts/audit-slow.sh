#!/usr/bin/env bash
# audit-slow.sh — Process one test file every N minutes (default: 5).
#
# Designed to run unattended for days, consuming Claude tokens gradually.
# Uses the existing audit-test-descriptions.py with --resume --limit 1.
#
# Usage:
#   ./scripts/audit-slow.sh                   # 5-min interval, all categories
#   ./scripts/audit-slow.sh --interval 10     # 10-min interval
#   ./scripts/audit-slow.sh --category coverage --retry-errors
#   ./scripts/audit-slow.sh --stop            # stop a running instance
#
# Logs to scripts/audit-slow.log. PID saved to scripts/audit-slow.pid.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PIDFILE="$SCRIPT_DIR/audit-slow.pid"
LOGFILE="$SCRIPT_DIR/audit-slow.log"

INTERVAL=300  # seconds (5 minutes)
CATEGORY=""
RETRY_ERRORS=""
TIMEOUT=120

usage() {
    cat <<'EOF'
Usage: audit-slow.sh [OPTIONS]

Options:
  --interval MINS    Minutes between audits (default: 5)
  --category CAT     Only audit this category (coverage, regress-issue, etc.)
  --retry-errors     Retry files that previously errored
  --timeout SECS     Per-file timeout in seconds (default: 120)
  --stop             Stop a running instance
  --status           Show status of running instance
  -h, --help         Show this help
EOF
}

stop_instance() {
    if [[ -f "$PIDFILE" ]]; then
        pid=$(cat "$PIDFILE")
        if kill -0 "$pid" 2>/dev/null; then
            echo "Stopping audit-slow (PID $pid)..."
            kill "$pid"
            rm -f "$PIDFILE"
            echo "Stopped."
        else
            echo "Stale PID file (process $pid not running). Removing."
            rm -f "$PIDFILE"
        fi
    else
        echo "No running instance found."
    fi
}

show_status() {
    if [[ -f "$PIDFILE" ]]; then
        pid=$(cat "$PIDFILE")
        if kill -0 "$pid" 2>/dev/null; then
            echo "audit-slow is running (PID $pid)"
            if [[ -f "$LOGFILE" ]]; then
                echo ""
                echo "Last 10 log lines:"
                tail -10 "$LOGFILE"
            fi
        else
            echo "Stale PID file (process $pid not running)."
        fi
    else
        echo "No running instance found."
    fi
    # Show counts from all result files
    echo ""
    echo "Result counts:"
    for f in "$SCRIPT_DIR"/audit-*.jsonl; do
        [[ -f "$f" ]] || continue
        base=$(basename "$f" .jsonl)
        total=$(wc -l < "$f" | tr -d ' ')
        ok=$(grep -c '"verdict": *"ok"' "$f" 2>/dev/null || echo 0)
        mismatch=$(grep -c '"verdict": *"mismatch"' "$f" 2>/dev/null || echo 0)
        error=$(grep -c '"verdict": *"error"' "$f" 2>/dev/null || echo 0)
        unclear=$(grep -c '"verdict": *"unclear"' "$f" 2>/dev/null || echo 0)
        echo "  $base: $total total (ok=$ok mismatch=$mismatch unclear=$unclear error=$error)"
    done
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --interval)  INTERVAL=$(( $2 * 60 )); shift 2 ;;
        --category)  CATEGORY="$2"; shift 2 ;;
        --retry-errors) RETRY_ERRORS="--retry-errors"; shift ;;
        --timeout)   TIMEOUT="$2"; shift 2 ;;
        --stop)      stop_instance; exit 0 ;;
        --status)    show_status; exit 0 ;;
        -h|--help)   usage; exit 0 ;;
        *)           echo "Unknown option: $1"; usage; exit 1 ;;
    esac
done

# Check for existing instance
if [[ -f "$PIDFILE" ]]; then
    pid=$(cat "$PIDFILE")
    if kill -0 "$pid" 2>/dev/null; then
        echo "Already running (PID $pid). Use --stop first."
        exit 1
    else
        rm -f "$PIDFILE"
    fi
fi

echo "Starting audit-slow: 1 file every $((INTERVAL / 60))m"
echo "  Category: ${CATEGORY:-all}"
echo "  Retry errors: ${RETRY_ERRORS:-no}"
echo "  Timeout: ${TIMEOUT}s"
echo "  Log: $LOGFILE"
echo "  PID file: $PIDFILE"

# Build the audit command
build_cmd() {
    local cmd=(python3 "$SCRIPT_DIR/audit-test-descriptions.py"
        --max-concurrent 1
        --resume
        --limit 1
        --timeout "$TIMEOUT"
    )
    [[ -n "$CATEGORY" ]] && cmd+=(--category "$CATEGORY")
    [[ -n "$RETRY_ERRORS" ]] && cmd+=($RETRY_ERRORS)
    echo "${cmd[@]}"
}

# Run in background
(
    trap 'rm -f "$PIDFILE"; exit 0' INT TERM

    echo $BASHPID > "$PIDFILE"

    iteration=0
    while true; do
        iteration=$((iteration + 1))
        timestamp=$(date '+%Y-%m-%d %H:%M:%S')

        echo "--- [$timestamp] Iteration $iteration ---" >> "$LOGFILE"

        cmd=$(build_cmd)
        if output=$($cmd 2>&1); then
            echo "$output" >> "$LOGFILE"
        else
            echo "$output" >> "$LOGFILE"
        fi

        # Check if there's anything left to do
        if echo "$output" | grep -q "Nothing to audit"; then
            echo "[$timestamp] All files audited. Stopping." >> "$LOGFILE"
            echo "All files audited. Stopping."
            rm -f "$PIDFILE"
            exit 0
        fi

        sleep "$INTERVAL"
    done
) &

disown

sleep 0.5
if [[ -f "$PIDFILE" ]]; then
    echo "Running in background (PID $(cat "$PIDFILE"))."
    echo "Use './scripts/audit-slow.sh --status' to check progress."
    echo "Use './scripts/audit-slow.sh --stop' to stop."
else
    echo "Failed to start. Check $LOGFILE"
fi
