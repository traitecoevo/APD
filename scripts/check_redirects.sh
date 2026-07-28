#!/usr/bin/env bash
# Content-negotiation and identifier-resolution matrix for w3id.org/APD.
#
# Run it before a deploy, keep the output, run it after, diff the two. Every line
# must be identical except where a change was intended. This is the check stage 6
# of plans/build-workflow-overhaul.md schedules as `redirects.yml`.
#
#   scripts/check_redirects.sh > before.txt
#   ... deploy ...
#   scripts/check_redirects.sh | diff before.txt -
#
# It talks to the live service, so it needs network and it tests what users
# actually get -- not what the repo contains.
set -uo pipefail

APD_VERSION="${APD_VERSION:-2.1.0}"

resolve() {  # url -> "<status> <final-path>"
  local url="$1" accept="${2:-}" args=(-s -o /dev/null -L)
  [[ -n "$accept" ]] && args+=(-H "Accept: $accept")
  printf '%s %s' \
    "$(curl "${args[@]}" -w '%{http_code}' "$url")" \
    "$(curl "${args[@]}" -w '%{url_effective}' "$url" | sed 's|https://traitecoevo.github.io/APD/||')"
}

echo "# Content negotiation"
for accept in text/turtle application/n-triples application/n-quads \
              application/ld+json text/html; do
  for path in "" /traits /glossary; do
    printf '%-22s %-26s -> %s\n' "$accept" "APD${path}" \
      "$(resolve "https://w3id.org/APD${path}" "$accept")"
  done
done

echo
echo "# Identifier resolution -- one per entity class"
for path in traits/trait_0000012 traits/trait_group_0000008 \
            traits/plant_growth_form_tree glossary/glossary_40004; do
  printf '%-40s -> %s\n' "$path" "$(resolve "https://w3id.org/APD/$path")"
done

echo
echo "# Versioned permalink -- published by index.qmd's \"This version\" link"
printf '%-40s -> %s\n' "release/${APD_VERSION}/index.html" \
  "$(resolve "https://w3id.org/APD/release/${APD_VERSION}/index.html")"

# w3id resolves any path it does not recognise as an entity to the dictionary page,
# which is the intended fallback. So the data files are fetched from github.io --
# see COMMITMENTS.md C12 -- and it is those URLs that have to serve data rather
# than HTML. A wrong one still returns 200, so check the body, not the status.
echo
echo "# Published data files -- must serve data, not the HTML page"
SITE="https://traitecoevo.github.io/APD"
for path in APD_traits.csv APD_categorical_values.csv APD.ttl APD.nt APD.nq \
            APD.json "release/${APD_VERSION}/APD_traits.csv" \
            "release/${APD_VERSION}/APD_categorical_values.csv"; do
  code="$(curl -s -o /dev/null -w '%{http_code}' -L "$SITE/$path")"
  body="$(curl -s -L "$SITE/$path" | head -c 24)"
  if [[ "$body" == *"<!DOCTYPE"* || "$body" == *"<html"* ]]; then
    printf '%-46s %s HTML -- WRONG\n' "$path" "$code"
  else
    printf '%-46s %s data\n' "$path" "$code"
  fi
done
