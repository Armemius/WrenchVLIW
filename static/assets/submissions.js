const key = 'wrench_submissions'

const colorClass = st => {
  switch (st) {
    case 'passed':
      return 'border border-green-500 text-green-500'
    case 'error':
      return 'border border-yellow-500 text-yellow-500'
    case 'failed':
      return 'border border-red-500 text-red-500'
    default:
      return 'border border-slate-500 text-slate-500'
  }
}

export const renderSubmissions = () => {
  const listEl = document.getElementById('submissions-list')
  if (!listEl) return

  const items =
    JSON.parse(localStorage.getItem(key) || '[]').filter(
      e => typeof e === 'object',
    ) ?? []

  if (!items.length) {
    listEl.innerHTML =
      '<div class="text-[var(--c-grey)]">No submissions yet.</div>'
    return
  }

  listEl.innerHTML = items
    .map(
      e => `
        <div class="flex items-center justify-between rounded px-3 py-2 bg-[var(--c-dark-grey)]">
          <div class="flex flex-col">
            <span class="text-sm text-[var(--c-grey)]">${new Date(
              e.timestamp,
            ).toLocaleString()}</span>
            <span class="text-base text-[var(--c-white)]">${e.isa || '-'} ${
              e.variant ? '(' + e.variant + ')' : ''
            }</span>
            <span class="text-xs text-[var(--c-grey)]">wrench ${
              e.wrenchVersion || ''
            }</span>
          </div>
          <div class="flex items-center gap-2">
          ${
            e.link
              ? `<div class="flex items-center gap-2 mr-2">
                   <button type="button" class="outline-link text-[var(--c-blue)] text-sm copy-submission" style="--link-color: var(--c-blue);" data-link="${e.link}">[copy_link]</button>
                   <button type="button" class="outline-link text-[var(--c-green)] text-sm open-submission" style="--link-color: var(--c-green);" data-link="${e.link}">[open]</button>
                 </div>`
              : ''
          }
            ${
              e.total > 0
                ? `<span class="text-sm text-[var(--c-white)]">${e.passed}/${e.total}</span>`
                : ''
            }
            <span class="px-2 py-1 rounded ${colorClass(e.status)}">${e.status}</span>
            
          </div>
        </div>
      `,
    )
    .join('')

  listEl.querySelectorAll('.copy-submission').forEach(btn =>
    btn.addEventListener('click', () => {
      const link = btn.dataset.link
      if (link) navigator.clipboard.writeText(link)
    }),
  )

  listEl.querySelectorAll('.open-submission').forEach(btn =>
    btn.addEventListener('click', () => {
      const link = btn.dataset.link
      if (link) window.open(link, '_blank')
    }),
  )
}

export const recordSubmission = entry => {
  const items =
    JSON.parse(localStorage.getItem(key) || '[]').filter(
      e => typeof e === 'object',
    ) ?? []

  const filtered = entry.link
    ? items.filter(e => e.link !== entry.link)
    : items.filter(
        e =>
          !(
            e.timestamp === entry.timestamp &&
            e.status === entry.status &&
            e.isa === entry.isa &&
            e.variant === entry.variant
          ),
      )

  filtered.unshift(entry)
  localStorage.setItem(key, JSON.stringify(filtered.slice(0, 50)))
}
export const submissionsKey = key
