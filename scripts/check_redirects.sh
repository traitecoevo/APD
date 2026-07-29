#!/usr/bin/env bash
# Does every published URI still land on its content?
#
# This talks to the live service, so it tests what a user actually gets rather
# than what the repo contains. That is the only way to catch the two failures
# nothing else here can see: a w3id rule that has drifted away from the site,
# and a deploy that quietly dropped a versioned permalink.
#
# Three severities, matching `make check`:
#
#   ok    the URI resolved to what it promises
#   gap   a resolution that is already broken in production and recorded in
#         COMMITMENTS.md. Reported every run, does not fail. If a gap starts
#         *passing* that does fail: the register has outlived its problem, and
#         both it and this script need updating.
#   FAIL  anything else. Exits non-zero.
#
# Run weekly, and after every deploy, by .github/workflows/redirects.yml. Also
# worth running by hand either side of a change to the w3id rules:
#
#   scripts/check_redirects.sh
#
# Set APD_VERSION to check a version other than the one in DESCRIPTION.
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SITE="https://traitecoevo.github.io/APD"
APD_VERSION="${APD_VERSION:-$(awk '/^Version:/ {print $2}' "$ROOT/DESCRIPTION")}"

failures=0
gaps=0

# Retry rather than report a third party's blip as a broken redirect: w3id.org
# is a community-run service and this runs unattended on a schedule.
CURL=(curl -sS -o /dev/null -L --retry 3 --retry-delay 2 --max-time 30)

# The HTML targets are the *directory* form -- /APD/#<slug>, not
# /APD/index.html#<slug>. Both serve the same document, so naming the second gave
# one page two URLs: two browser cache entries, and a search result clicked from
# /APD/ reloaded 6 MB instead of jumping. See perma-id/w3id.org#6454 and #60.
#
# resolve() strips the site root but keeps the leading '/', so the bare document
# reads as "/" rather than an empty string.
resolve() {  # url [accept] -> "<status> <final-path>"
  local url="$1" accept="${2:-}" args=("${CURL[@]}")
  [[ -n "$accept" ]] && args+=(-H "Accept: $accept")
  printf '%s %s' \
    "$("${args[@]}" -w '%{http_code}' "$url")" \
    "$("${args[@]}" -w '%{url_effective}' "$url" | sed "s|$SITE||")"
}

expect() {  # label actual expected [known-gap-explanation]
  local label="$1" actual="$2" expected="$3" gap="${4:-}"

  if [[ "$actual" == "$expected" ]]; then
    if [[ -z "$gap" ]]; then
      printf '[  ok  ] %-44s %s\n' "$label" "$actual"
      return
    fi
    # A recorded gap that now resolves correctly. Good news, and still a
    # failure: a register entry that outlives its problem silences a check.
    printf '[ FAIL ] %-44s %s\n' "$label" "$actual"
    printf '         fixed! remove this expected failure from COMMITMENTS.md\n'
    printf '         and from this script: %s\n' "$gap"
    failures=$((failures + 1))
  elif [[ -n "$gap" ]]; then
    printf '[ gap  ] %-44s %s\n' "$label" "$actual"
    printf '         want %s -- %s\n' "$expected" "$gap"
    gaps=$((gaps + 1))
  else
    printf '[ FAIL ] %-44s %s\n' "$label" "$actual"
    printf '         want %s\n' "$expected"
    failures=$((failures + 1))
  fi
}

section() { printf '\n%s\n' "$1"; }


# --- content negotiation -----------------------------------------------------
#
# w3id.org/APD serves whichever serialisation the Accept header asks for. These
# are the lines that must never change: they are the machine-readable interface
# COMMITMENTS.md C5 and C12 promise.

section "Content negotiation"

for spec in "text/turtle:/APD.ttl" "application/n-triples:/APD.nt" \
            "application/n-quads:/APD.nq" "application/ld+json:/APD.json"; do
  accept="${spec%%:*}"
  want="${spec##*:}"
  for path in "" /traits /glossary; do
    expect "$accept APD$path" \
      "$(resolve "https://w3id.org/APD$path" "$accept")" "200 $want"
  done
done

# The document itself, and the two collection URIs, which land on their section
# of it rather than its top.
for spec in ":/" "/traits:/#trait-concepts" \
            "/glossary:/#glossary"; do
  path="${spec%%:*}"
  want="${spec#*:}"
  expect "text/html APD$path" \
    "$(resolve "https://w3id.org/APD$path" "text/html")" "200 $want"
done


# --- identifier resolution ---------------------------------------------------
#
# One per entity class. COMMITMENTS.md C1 promises that each of the 1,473
# identifiers resolves to that term's content; the anchors themselves are
# checked at render time by scripts/build_site.R, so what is left to test here
# is whether w3id sends each class to the right fragment.

section "Identifier resolution -- one per entity class"

# The categorical value is the one that used to fail: the rule matched only
# `trait_`, so all 819 fell through to the catch-all and landed at the top of the
# page. The stage 5 rule change widened it to `^traits/([^/]+)/?$`, and this line
# is what proves it stayed fixed. `heat+smoke` is here because `+` is the one slug
# character a narrower character class would have missed.
for spec in "traits/trait_0000012:trait_0000012" \
            "traits/trait_group_0000008:trait_group_0000008" \
            "traits/plant_growth_form_tree:plant_growth_form_tree" \
            "traits/seed_germination_treatment_heat+smoke:seed_germination_treatment_heat+smoke" \
            "glossary/glossary_40004:glossary_40004"; do
  path="${spec%%:*}"
  anchor="${spec##*:}"
  expect "$path" "$(resolve "https://w3id.org/APD/$path")" \
    "200 /#$anchor"
done


# --- versioned permalinks ----------------------------------------------------
#
# Every snapshot in release/ is a permalink someone may have cited -- index.qmd
# publishes the current and previous ones as "This version" / "Previous
# version", and Zenodo deposits point at them. They are served from the deployed
# site, so a deploy that stops carrying release/ silently 404s all of them. That
# is the failure the stage 6 gate exists to catch.

# Requested in the legacy `index.html` form on purpose: that is what index.qmd
# published as "This version" and what the Zenodo deposits cite, so it is the form
# most likely to be in someone's bibliography. It has to keep resolving, and it
# should land on the canonical directory URL.
section "Versioned permalinks -- every snapshot in release/"

for dir in "$ROOT"/release/*/; do
  version="$(basename "$dir")"
  expect "release/$version/index.html" \
    "$(resolve "https://w3id.org/APD/release/$version/index.html")" \
    "200 /release/$version/"
done


# --- published data files ----------------------------------------------------
#
# w3id resolves any path it does not recognise as an entity to the dictionary
# page, so a wrong URL here still returns 200 -- check the body, not the status.
# These are the URLs austraits.build and using_the_APD.qmd read: COMMITMENTS.md
# C12.

section "Published data files -- must serve data, not the HTML page"

for path in APD_traits.csv APD_categorical_values.csv APD.ttl APD.nt APD.nq \
            APD.json "release/${APD_VERSION}/APD_traits.csv" \
            "release/${APD_VERSION}/APD_categorical_values.csv"; do
  code="$("${CURL[@]}" -w '%{http_code}' "$SITE/$path")"
  # `-s` rather than `-sS` here: `head` closes the pipe after 24 bytes and curl
  # would report the resulting SIGPIPE as an error on every one of these.
  body="$(curl -s -L --retry 3 --max-time 30 "$SITE/$path" | head -c 24)"
  if [[ "$body" == *"<!DOCTYPE"* || "$body" == *"<html"* ]]; then
    actual="$code HTML"
  else
    actual="$code data"
  fi
  expect "$path" "$actual" "200 data"
done


# --- summary -----------------------------------------------------------------

printf '\n%s\n' "$(printf '%.0s-' {1..78})"

if (( gaps > 0 )); then
  printf '%d known gap(s), tracked in COMMITMENTS.md.\n' "$gaps"
fi

if (( failures > 0 )); then
  printf '\n%d check(s) FAILED.\n' "$failures"
  exit 1
fi

if (( gaps > 0 )); then
  printf '\nNo failures.\n'
else
  printf 'All checks passed.\n'
fi
