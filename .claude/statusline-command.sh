#!/bin/bash

# Read JSON input
input=$(cat)

# === Current session ===
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // 0')
total_input=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
total_output=$(echo "$input" | jq -r '.context_window.total_output_tokens // 0')

session_cost=$(echo "scale=4; $total_input * 15 / 1000000 + $total_output * 75 / 1000000" | bc)

# === Current working directory ===
cwd=$(echo "$input" | jq -r '.cwd // empty')

# === Today's cost (cached, refreshed in background every 5 min) ===
CACHE_FILE="/tmp/claude-today-cost"
LOCK_FILE="/tmp/claude-today-cost.lock"
CACHE_TTL=300

refresh_today_cost() {
  # Skip if another refresh is already running
  if ! mkdir "$LOCK_FILE" 2>/dev/null; then
    return
  fi

  # Midnight UTC today
  local cutoff
  cutoff=$(date -u +%Y-%m-%dT00:00:00)

  # Find recent JSONL files, grep for assistant messages, compute cost per model
  local total
  total=$(find ~/.claude/projects -name "*.jsonl" -mmin -1440 2>/dev/null | \
    xargs grep -h '"type":"assistant"' 2>/dev/null | \
    jq -r --arg cutoff "$cutoff" '
      select(.type == "assistant" and .message.usage != null and .timestamp > $cutoff) |
      .message as $m |
      (if ($m.model | tostring | test("opus")) then
        [15, 75, 18.75, 1.50]
      elif ($m.model | tostring | test("sonnet")) then
        [3, 15, 3.75, 0.30]
      elif ($m.model | tostring | test("haiku")) then
        [0.80, 4, 1, 0.08]
      else
        [15, 75, 18.75, 1.50]
      end) as $r |
      (($m.usage.input_tokens // 0) * $r[0] +
       ($m.usage.output_tokens // 0) * $r[1] +
       ($m.usage.cache_creation_input_tokens // 0) * $r[2] +
       ($m.usage.cache_read_input_tokens // 0) * $r[3]) / 1000000
    ' 2>/dev/null | awk '{s+=$1} END {printf "%.4f", s}')

  echo "${total:-0.0000}" > "$CACHE_FILE"
  rmdir "$LOCK_FILE" 2>/dev/null
}

# Check if cache needs refresh
need_refresh=false
if [[ ! -f "$CACHE_FILE" ]]; then
  need_refresh=true
else
  cache_mod=$(stat -f %m "$CACHE_FILE" 2>/dev/null || stat -c %Y "$CACHE_FILE" 2>/dev/null)
  now=$(date +%s)
  if (( now - cache_mod > CACHE_TTL )); then
    need_refresh=true
  fi
fi

if $need_refresh; then
  refresh_today_cost &
fi

# Read cached value
cost_today=$(cat "$CACHE_FILE" 2>/dev/null || echo "...")

# === Progress bar ===
bar_width=20
filled=$(printf "%.0f" $(echo "scale=0; $used_pct * $bar_width / 100" | bc))
empty=$((bar_width - filled))

progress_bar="["
for ((i=0; i<filled; i++)); do progress_bar+="█"; done
for ((i=0; i<empty; i++)); do progress_bar+="░"; done
progress_bar+="]"

# === Output ===
printf "\033[36mContext: %s %.0f%%\033[0m | \033[32mSession: \$%.4f\033[0m | \033[33mToday: \$%s\033[0m | 📁 %s\n" \
  "$progress_bar" "$used_pct" "$session_cost" "$cost_today" "$cwd"
