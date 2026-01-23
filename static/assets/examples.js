const friendlyName = path => {
  if (!path) return ''
  const parts = path.split(/[\\/]/)
  return parts[parts.length - 1]
}

const clearList = (listEl, statusEl, message) => {
  statusEl.textContent = message
  listEl.innerHTML = `<div class="text-[var(--c-grey)]">${message}</div>`
}

export async function renderExamples() {
  const listEl = document.getElementById('examples-list')
  const statusEl = document.getElementById('examples-status')

  if (!listEl || !statusEl) return

  try {
    const res = await fetch('/assets/examples.json', { cache: 'no-cache' })
    if (!res.ok) {
      clearList(listEl, statusEl, 'Examples were not prebuilt for this build.')
      return
    }

    const data = await res.json()
    const examples = data.examples ?? []
    const builtAt = data.generated_at

    if (!examples.length) {
      clearList(listEl, statusEl, 'No examples found.')
      return
    }

    statusEl.textContent = `Generated ${builtAt || ''} (${examples.length} total)`
    listEl.innerHTML = examples
      .map(ex => {
        const report = ex.report || (ex.guid ? `/report/${ex.guid}` : null)
        const name = ex.title || ex.id || friendlyName(ex.asm)
        const asm = ex.asm || ''
        const config = ex.config || ''
        const isa = ex.isa || ''
        const asmBase = friendlyName(asm).replace(/\.s$/i, '')
        const cfgBase = friendlyName(config).replace(/\.ya?ml$/i, '')
        const configNote =
          asmBase && cfgBase && asmBase !== cfgBase ? ` (${cfgBase})` : ''
        return `
          <div class="flex items-center justify-between rounded px-3 py-2 bg-[var(--c-dark-grey)]">
            <div class="flex flex-col">
              <span class="text-sm text-[var(--c-grey)]">${isa}</span>
              <span class="text-base text-[var(--c-white)]">${name}${configNote}</span>
              <span class="text-xs text-[var(--c-grey)]">asm: ${asm}</span>
              <span class="text-xs text-[var(--c-grey)]">config: ${config}</span>
            </div>
            <div class="flex items-center gap-2">
              ${
                report
                  ? `<div class="flex items-center gap-2">
                       <button type="button" class="outline-link text-[var(--c-green)] text-sm open-example" style="--link-color: var(--c-green);" data-link="${report}">[open]</button>
                     </div>`
                  : '<span class="text-[var(--c-grey)] text-sm">report missing</span>'
              }
            </div>
          </div>
        `
      })
      .join('')

    listEl.querySelectorAll('.copy-example').forEach(btn =>
      btn.addEventListener('click', () => {
        const link = btn.dataset.link
        if (link) navigator.clipboard.writeText(link)
      }),
    )

    listEl.querySelectorAll('.open-example').forEach(btn =>
      btn.addEventListener('click', () => {
        const link = btn.dataset.link
        if (link) window.open(link, '_blank')
      }),
    )
  } catch (_err) {
    clearList(listEl, statusEl, 'Examples were not prebuilt for this build.')
  }
}
