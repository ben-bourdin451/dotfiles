#!/bin/sh
# tmux status line metrics: CPU utilisation, memory used, network throughput.
#
# Usage: tmux-status.sh cpu|mem|net
#
# Portable across macOS (BSD userland) and Linux (procfs). Each metric prints a
# single fixed-width token on stdout, or "--" when it cannot be sampled yet.
# CPU and network are rates, so they need a previous sample: the first call
# after a reload always prints "--".
#
# Output is padded to a constant width so a centred window list does not jitter.
#
# No configuration and no host-identifying data is stored: the network
# interface is resolved from the default route at runtime, and sample state
# lives in a per-user temp dir, never in the repo.

set -u

OS=$(uname -s)

STATE_DIR="${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}/tmux-status-$(id -u)"
mkdir -p "$STATE_DIR" 2>/dev/null || true
chmod 700 "$STATE_DIR" 2>/dev/null || true

UNKNOWN='--'
CPU_LABEL='cpu|'
MEM_LABEL='mem|'

# High-resolution epoch seconds.
now() {
	_t=$(date +%s.%N 2>/dev/null)
	# busybox date does not implement %N and drops it, leaving a trailing dot.
	case "$_t" in
		*.) _t="${_t}0" ;;
	esac
	# BSD date before macOS 15 emits %N literally; fall back to whole seconds.
	case "$_t" in
		'' | *[!0-9.]*) date +%s ;;
		*) printf '%s\n' "$_t" ;;
	esac
}

ncpu() {
	if [ "$OS" = Darwin ]; then
		sysctl -n hw.ncpu 2>/dev/null
	elif command -v nproc >/dev/null 2>&1; then
		nproc
	else
		getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1
	fi
}

# Read the previous sample, write the new one, echo the previous
# (empty on the first call).
roll_state() {
	_f="$STATE_DIR/$1"
	_old=$(cat "$_f" 2>/dev/null || true)
	printf '%s\n' "$2" >"$_f" 2>/dev/null || true
	printf '%s\n' "$_old"
}

# A labelled percentage, or "--", at a constant width. $1 is the label prefix,
# $2 the value. The label sits flush against its value and the pad goes on the
# end, so "7%" and "100%" occupy the same columns without opening a gap after
# the label. Width is the label plus four columns, the width of "100%".
pct() {
	_label="$1"
	case "${2:-}" in
		'' | "$UNKNOWN") _text="$_label$UNKNOWN" ;;
		*) _text="$_label$2%" ;;
	esac
	_want=$(( ${#_label} + 4 ))
	while [ "${#_text}" -lt "$_want" ]; do
		_text="$_text "
	done
	printf '%s\n' "$_text"
}

##
## CPU — percentage of total capacity in use since the previous sample
##

# Linux: cumulative jiffies from /proc/stat, which is its own time base.
# Elsewhere: summed per-process CPU time, which needs a wall-clock delta.
cpu_sample() {
	if [ -r /proc/stat ]; then
		awk '$1=="cpu"{idle=$5+$6; total=0; for(i=2;i<=NF;i++) total+=$i; print idle, total; exit}' /proc/stat
	else
		# BSD `ps -o cputime` prints [HH:]MM:SS.ss with minutes unbounded.
		_cpu=$(ps -A -o cputime= 2>/dev/null | awk -F: '
			{ if (NF >= 3) s += $1*3600 + $2*60 + $3; else if (NF == 2) s += $1*60 + $2 }
			END { printf "%.2f", s+0 }')
		[ -n "$_cpu" ] || return 1
		printf '%s %s\n' "$_cpu" "$(now)"
	fi
}

cmd_cpu() {
	new=$(cpu_sample) || { pct "$CPU_LABEL" "$UNKNOWN"; return; }
	[ -n "$new" ] || { pct "$CPU_LABEL" "$UNKNOWN"; return; }
	old=$(roll_state cpu "$new")
	[ -n "$old" ] || { pct "$CPU_LABEL" "$UNKNOWN"; return; }

	if [ -r /proc/stat ]; then
		value=$(printf '%s\n%s\n' "$old" "$new" | awk -v unknown="$UNKNOWN" '
			NR==1 { oidle=$1; ototal=$2 }
			NR==2 { nidle=$1; ntotal=$2 }
			END {
				dt = ntotal - ototal; di = nidle - oidle
				if (dt <= 0 || di < 0) { print unknown; exit }
				u = 100 * (1 - di/dt)
				if (u < 0) u = 0; if (u > 100) u = 100
				printf "%.0f\n", u
			}')
	else
		value=$(printf '%s\n%s\n' "$old" "$new" | awk -v cores="$(ncpu)" -v unknown="$UNKNOWN" '
			NR==1 { ocpu=$1; otime=$2 }
			NR==2 { ncpu_=$1; ntime=$2 }
			END {
				dt = ntime - otime; dc = ncpu_ - ocpu
				if (dt <= 0 || dc < 0 || cores <= 0) { print unknown; exit }
				u = 100 * dc / (dt * cores)
				if (u < 0) u = 0; if (u > 100) u = 100
				printf "%.0f\n", u
			}')
	fi
	pct "$CPU_LABEL" "$value"
}

##
## Memory — percentage in use, excluding reclaimable cache
##

cmd_mem() {
	if [ -r /proc/meminfo ]; then
		# MemAvailable already accounts for reclaimable page cache and slab.
		value=$(awk -v unknown="$UNKNOWN" '
			/^MemTotal:/     { total = $2 }
			/^MemAvailable:/ { avail = $2; have_avail = 1 }
			/^MemFree:/      { free = $2 }
			/^Buffers:/      { buffers = $2 }
			/^Cached:/       { cached = $2 }
			END {
				if (total <= 0) { print unknown; exit }
				if (!have_avail) avail = free + buffers + cached
				printf "%.0f\n", 100 * (total - avail) / total
			}' /proc/meminfo)
	elif [ "$OS" = Darwin ]; then
		# Matches Activity Monitor "Memory Used": app memory + wired + compressed.
		# Inactive and speculative pages are reclaimable and deliberately excluded.
		_total=$(sysctl -n hw.memsize 2>/dev/null)
		_pagesize=$(sysctl -n hw.pagesize 2>/dev/null)
		if [ -z "$_total" ] || [ -z "$_pagesize" ]; then
			pct "$MEM_LABEL" "$UNKNOWN"
			return
		fi
		value=$(vm_stat 2>/dev/null | awk -v total="$_total" -v pagesize="$_pagesize" -v unknown="$UNKNOWN" '
			/^Pages active:/          { active = $3 }
			/^Pages wired down:/      { wired = $4 }
			/occupied by compressor:/ { compressed = $5 }
			/^Pages purgeable:/       { purgeable = $3 }
			END {
				gsub(/\./, "", active); gsub(/\./, "", wired)
				gsub(/\./, "", compressed); gsub(/\./, "", purgeable)
				if (total <= 0) { print unknown; exit }
				used = (active + wired + compressed - purgeable) * pagesize
				if (used < 0) used = 0
				u = 100 * used / total
				if (u > 100) u = 100
				printf "%.0f\n", u
			}')
	else
		value="$UNKNOWN"
	fi
	pct "$MEM_LABEL" "$value"
}

##
## Network — throughput on the current default-route interface
##

default_iface() {
	if [ "$OS" = Darwin ]; then
		route -n get default 2>/dev/null | awk '/interface:/{print $2; exit}'
		return
	fi
	# Prefer procfs: always present on Linux, needs no iproute2/net-tools.
	# Destination 00000000 is the default route; pick the lowest metric.
	if [ -r /proc/net/route ]; then
		_if=$(awk '$2 == "00000000" && (best == "" || $7 < best) { best = $7; iface = $1 }
		           END { if (iface != "") print iface }' /proc/net/route)
		if [ -n "$_if" ]; then
			printf '%s\n' "$_if"
			return
		fi
	fi
	if command -v ip >/dev/null 2>&1; then
		ip route show default 2>/dev/null |
			awk '{for(i=1;i<=NF;i++) if($i=="dev"){print $(i+1); exit}}'
	else
		route -n 2>/dev/null | awk '$1=="0.0.0.0"{print $NF; exit}'
	fi
}

# Cumulative rx/tx bytes for interface $1.
net_bytes() {
	if [ -r "/sys/class/net/$1/statistics/rx_bytes" ]; then
		_rx=$(cat "/sys/class/net/$1/statistics/rx_bytes" 2>/dev/null)
		_tx=$(cat "/sys/class/net/$1/statistics/tx_bytes" 2>/dev/null)
		[ -n "$_rx" ] && [ -n "$_tx" ] && printf '%s %s\n' "$_rx" "$_tx"
	else
		# BSD netstat repeats a row per address family; the <Link#N> row carries
		# the byte counters. Fall back to the first row if it is absent.
		netstat -ibn -I "$1" 2>/dev/null | awk -v iface="$1" '
			$1 == iface && $3 ~ /^<Link/ { print $7, $10; found = 1; exit }
			END { if (!found) exit 1 }' ||
			netstat -ibn -I "$1" 2>/dev/null | awk -v iface="$1" '$1 == iface { print $7, $10; exit }'
	fi
}

# Bytes per second, never wider than four columns: step up a unit before the
# mantissa would need four digits, and drop the decimal once it needs two.
fmt_rate() {
	awk -v b="$1" 'BEGIN {
		if (b < 0) b = 0
		split("B K M G T", unit, " ")
		i = 1; v = b
		while (v >= 999.5 && i < 5) { v /= 1024; i++ }
		if (i == 1)        printf "%.0f%s", v, unit[i]
		else if (v < 9.95) printf "%.1f%s", v, unit[i]
		else               printf "%.0f%s", v, unit[i]
	}'
}

net_line() {
	printf '%4s↓ %4s↑\n' "$1" "$2"
}

cmd_net() {
	iface=$(default_iface)
	[ -n "$iface" ] || { net_line "$UNKNOWN" "$UNKNOWN"; return; }

	counters=$(net_bytes "$iface")
	[ -n "$counters" ] || { net_line "$UNKNOWN" "$UNKNOWN"; return; }

	new="$iface $counters $(now)"
	old=$(roll_state net "$new")
	[ -n "$old" ] || { net_line "$UNKNOWN" "$UNKNOWN"; return; }

	# A different interface (docked, VPN up) means the counters are unrelated.
	case "$old" in
		"$iface "*) ;;
		*) net_line "$UNKNOWN" "$UNKNOWN"; return ;;
	esac

	rates=$(printf '%s\n%s\n' "$old" "$new" | awk '
		NR==1 { orx=$2; otx=$3; otime=$4 }
		NR==2 { nrx=$2; ntx=$3; ntime=$4 }
		END {
			dt = ntime - otime
			# Counters can wrap or reset when an interface is reconfigured.
			if (dt <= 0 || nrx < orx || ntx < otx) exit 1
			printf "%d %d\n", (nrx-orx)/dt, (ntx-otx)/dt
		}') || { net_line "$UNKNOWN" "$UNKNOWN"; return; }

	net_line "$(fmt_rate "${rates% *}")" "$(fmt_rate "${rates#* }")"
}

case "${1:-}" in
	cpu) cmd_cpu ;;
	mem) cmd_mem ;;
	net) cmd_net ;;
	*)
		echo "usage: ${0##*/} cpu|mem|net" >&2
		exit 2
		;;
esac
