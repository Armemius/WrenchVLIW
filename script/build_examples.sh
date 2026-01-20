#!/usr/bin/env bash
set -euo pipefail

EXAMPLE_ROOT=${EXAMPLE_ROOT:-/app/example}
EXAMPLE_OUTPUT=${EXAMPLE_OUTPUT:-/app/example-reports}
EXAMPLES_JSON=${EXAMPLES_JSON:-/app/static/assets/examples.json}
EXAMPLE_PORT=${EXAMPLE_PORT:-8090}
WRENCH_SERV_BIN=${WRENCH_SERV_BIN:-/app/.local/bin/wrench-serv}
WRENCH_BIN=${WRENCH_BIN:-/app/.local/bin/wrench}
VARIANTS=${VARIANTS:-/app/variants}

mkdir -p "$(dirname "$EXAMPLES_JSON")"
rm -rf "$EXAMPLE_OUTPUT"
mkdir -p "$EXAMPLE_OUTPUT"

EXAMPLES_LIST=$(mktemp)

common_prefix() {
    local a="${1-}"
    local b="${2-}"
    local i=0
    local limit=${#a}

    [ ${#b} -lt $limit ] && limit=${#b}
    while [ $i -lt $limit ] && [ "${a:$i:1}" = "${b:$i:1}" ]; do
        i=$((i + 1))
    done
    echo "${a:0:$i}"
}

discover_examples() {
    local root="$1"
    find "$root" -type d | while read -r dir; do
        [ "$dir" = "$root" ] && continue
        isa=${dir#"$root"/}
        isa=${isa%%/*}
        [ -z "$isa" ] && continue

        shopt -s nullglob
        asms=("$dir"/*.s)
        yamls=("$dir"/*.yaml)
        shopt -u nullglob

        [ ${#asms[@]} -eq 0 ] && continue
        [ ${#yamls[@]} -eq 0 ] && continue

        for asm in "${asms[@]}"; do
            asm_base=${asm##*/}
            asm_base=${asm_base%.s}
            for yml in "${yamls[@]}"; do
                yml_base=${yml##*/}
                yml_base=${yml_base%.yaml}
                prefix=$(common_prefix "$asm_base" "$yml_base")
                [ ${#prefix} -lt 3 ] && continue
                printf "%s|%s|%s|%s\n" "$isa" "$asm" "$yml" "$asm_base"
            done
        done
    done | sort -u
}

discover_examples "$EXAMPLE_ROOT" >"$EXAMPLES_LIST"

mapfile -t EXAMPLE_PAIRS <"$EXAMPLES_LIST"

if [ "${#EXAMPLE_PAIRS[@]}" -eq 0 ]; then
    echo "No examples discovered under ${EXAMPLE_ROOT}" >&2
    now=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    cat >"$EXAMPLES_JSON" <<EOF
{
  "generated_at": "${now}",
  "examples": []
}
EOF
    exit 0
fi

echo "Discovered ${#EXAMPLE_PAIRS[@]} example pairs"

STORAGE_PATH="$EXAMPLE_OUTPUT" PORT="$EXAMPLE_PORT" VARIANTS="$VARIANTS" WRENCH_EXEC="$WRENCH_BIN" "$WRENCH_SERV_BIN" >/tmp/wrench-serv.log 2>&1 &
SERVER_PID=$!
cleanup() {
    if kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

for _ in $(seq 1 30); do
    if curl -fs "http://127.0.0.1:${EXAMPLE_PORT}/submit-form" >/dev/null 2>&1; then
        break
    fi
    sleep 1
done

if ! curl -fs "http://127.0.0.1:${EXAMPLE_PORT}/submit-form" >/dev/null 2>&1; then
    echo "wrench-serv failed to start for example generation" >&2
    exit 1
fi

RESULTS_FILE=$(mktemp)

for line in "${EXAMPLE_PAIRS[@]}"; do
    IFS='|' read -r ISA ASM CFG TITLE <<<"$line"
    echo "Running example ${ASM} with ${CFG}"
    headers=$(curl -s -o /dev/null -D - \
        --data-urlencode "name=${TITLE}" \
        --data-urlencode "comment=example:${ISA}" \
        --data-urlencode "variant=" \
        --data-urlencode "isa=${ISA}" \
        --data-urlencode "asm@${ASM}" \
        --data-urlencode "config@${CFG}" \
        "http://127.0.0.1:${EXAMPLE_PORT}/submit")

    location=$(printf "%s" "$headers" | tr -d '\r' | awk '/^Location:/ {print $2}' | tail -n1)
    guid=${location##*/}

    if [ -z "$guid" ]; then
        echo "Failed to capture guid for ${ASM} (${CFG})" >&2
        continue
    fi

    printf "%s\t%s\t%s\t%s\t%s\t%s\n" "$ISA" "$ASM" "$CFG" "$TITLE" "$guid" "$location" >>"$RESULTS_FILE"
done

now=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
mkdir -p "$(dirname "$EXAMPLES_JSON")"
{
    echo "{"
    echo "  \"generated_at\": \"${now}\","
    echo "  \"examples\": ["
    first=true
    while IFS=$'\t' read -r isa asm cfg title guid location; do
        [ -z "$guid" ] && continue
        rel_asm=${asm#/app/}
        rel_cfg=${cfg#/app/}
        if [ "$first" = false ]; then
            echo "    ,"
        fi
        first=false
        cat <<EOF
    {
      "id": "${isa}:${title}",
      "title": "${title}",
      "isa": "${isa}",
      "asm": "${rel_asm}",
      "config": "${rel_cfg}",
      "guid": "${guid}",
      "report": "${location}"
    }
EOF
    done <"$RESULTS_FILE"
    echo "  ]"
    echo "}"
} >"$EXAMPLES_JSON"
