# hyperoptic-zyxel-wifi

Re-applies my Wi-Fi SSID + password to my **Hyperoptic EX3301-T0** (Zyxel-built) router after a
factory reset, by driving its web configurator with Playwright.

## Why a script?

The router UI is a Vue SPA that encrypts credentials client-side, so it can't be scripted with
plain `curl`. The script also handles two firmware quirks:

1. **Mask-widget inputs** — the login and Wi-Fi password fields ignore programmatic value-setting
   and only register real keystrokes, so the script types them character-by-character.
2. **Cosmetic password policy** — the UI's `checkPasswordStrenth` (sic) function forces a
   "must contain an uppercase letter" rule (pulled from `guiFlag.InputCheckList`). This is a
   client-side-only policy; WPA2/WPA3-PSK itself accepts any 8–63 char passphrase. The script
   overrides that one function at runtime so my chosen passphrase is accepted. The real
   validators (`wpapskCheck`, `isValidWiFiPskValue`) still run and only enforce length + charset.

## Setup (once)

```sh
cp .env.example .env      # then edit .env with real values (.env is gitignored)
```

Playwright is installed automatically on first run (browsers are cached system-wide).

## Usage

```sh
./run.sh                 # apply the SSID/password from .env
DRY=1 ./run.sh           # dry run: fill + verify, then Cancel (changes nothing)
./run.sh --reconnect     # apply, then rejoin this Mac to the new SSID (macOS)
HEADLESS=1 ./run.sh      # run the browser invisibly
```

Applying restarts the radios, so **all Wi-Fi clients disconnect** and must reconnect with the
new password.

## Files

| File           | Committed? | Purpose                                          |
|----------------|-----------|--------------------------------------------------|
| `set-wifi.js`  | yes       | The Playwright automation                        |
| `run.sh`       | yes       | Loads `.env`, ensures Playwright, runs + reconnects |
| `.env.example` | yes       | Template with placeholders                        |
| `.env`         | **no** (gitignored) | Real router/Wi-Fi credentials          |
| `package.json` | yes       | Declares the Playwright dependency               |
