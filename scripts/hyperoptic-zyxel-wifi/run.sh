#!/usr/bin/env bash
# Reset the router Wi-Fi to the values in .env. Usage:
#   ./run.sh            # apply the change
#   DRY=1 ./run.sh      # dry run: fill + verify + cancel (no changes)
#   ./run.sh --reconnect   # after applying, rejoin this Mac to the new SSID (macOS)
set -euo pipefail
cd "$(dirname "$0")"

if [[ ! -f .env ]]; then
  echo "No .env found. Copy .env.example to .env and fill in your values:"
  echo "  cp .env.example .env && \${EDITOR:-nano} .env"
  exit 1
fi
set -a; source ./.env; set +a

# Ensure Playwright is available locally (browsers are cached system-wide, so this is quick).
if [[ ! -d node_modules/playwright ]]; then
  echo "[run] installing playwright locally…"
  npm install --no-fund --no-audit playwright >/dev/null 2>&1
fi

RECONNECT=0
[[ "${1:-}" == "--reconnect" ]] && RECONNECT=1

node set-wifi.js

if [[ "$RECONNECT" == "1" && "${DRY:-0}" != "1" ]]; then
  echo "[run] waiting for radios to come back, then rejoining ${WIFI_SSID}…"
  # give the AP time to restart and rebroadcast
  for _ in $(seq 1 12); do
    if /usr/sbin/networksetup -setairportnetwork en0 "$WIFI_SSID" "$WIFI_PASS" 2>/dev/null \
       && ping -c1 -t2 "${ROUTER_HOST:-192.168.1.1}" >/dev/null 2>&1; then
      echo "[run] reconnected to ${WIFI_SSID}."
      exit 0
    fi
    sleep 8
  done
  echo "[run] could not auto-rejoin yet — connect to ${WIFI_SSID} manually."
fi
