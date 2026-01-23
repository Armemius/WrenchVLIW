const escapeHtml = text =>
  String(text || '')
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')

const normalizeRawText = text => {
  if (!text) return ''
  const trimmedStart = text.replace(/^[\\s\\t]*\\n/, '')
  return trimmedStart.replace(/\\n[\\s\\t]*$/, '')
}

const buildCodeBlock = (container, rawText) => {
  if (!container) return
  const text = normalizeRawText(rawText ?? container.textContent ?? '')
  container.dataset.raw = text
  container.innerHTML = ''

  const wrap = document.createElement('div')
  wrap.className = 'code-container block'

  const scroller = document.createElement('div')
  scroller.className = 'code-scroll'

  const lineNums = document.createElement('div')
  lineNums.className = 'line-numbers'

  const codeContent = document.createElement('div')
  codeContent.className = 'code-content'

  const lines = text.replace(/\r\n/g, '\n').split('\n')
  lines.forEach((line, idx) => {
    const ln = document.createElement('div')
    ln.className = 'line-number'
    ln.textContent = String(idx + 1)
    lineNums.appendChild(ln)

    const codeLine = document.createElement('div')
    codeLine.className = 'code-line'
    codeLine.setAttribute('role', 'presentation')
    codeLine.dataset.line = String(idx + 1)

    const textSpan = document.createElement('span')
    textSpan.className = 'code-line-text'
    textSpan.textContent = line
    codeLine.appendChild(textSpan)
    codeContent.appendChild(codeLine)
  })

  scroller.appendChild(lineNums)
  scroller.appendChild(codeContent)
  wrap.appendChild(scroller)
  container.appendChild(wrap)
}

const renderDebugNav = () => {
  const container = document.getElementById('debug-nav-container')
  const link = document.getElementById('debug-link')?.textContent?.trim()
  if (!container || !link) return

  const sep = document.createElement('span')
  const anchor = document.createElement('a')
  anchor.href = link
  anchor.className =
    'hover:bg-[var(--c-yellow)] pt-[0.2ch] pb-[0.2ch] text-[var(--c-yellow)] hover:text-[var(--c-black)] cursor-pointer'
  anchor.textContent = '[debug]'

  container.appendChild(sep)
  container.appendChild(anchor)
}

const getReportGuid = () => {
  const el = document.getElementById('report-guid')
  const fromEl = el?.textContent?.trim()
  if (fromEl) return fromEl
  const parts = window.location.pathname.split('/').filter(Boolean)
  return parts[parts.length - 1] || ''
}

const statusClass = entry => {
  if (entry.tceExitCode === 0 && entry.tceSuccess === true) {
    return 'status-outline status-success'
  }
  if (entry.tceExitCode === 2) return 'status-outline status-warning'
  return 'status-outline status-error'
}

const statusBadge = entry => {
  if (entry.tceExitCode === 0 && entry.tceSuccess === true) return 'passed'
  if (entry.tceExitCode === 2) return 'error'
  return 'failed'
}

const renderTestCaseCards = () => {
  const container = document.getElementById('test-cases-cards')
  if (!container) return

  let entries = []
  try {
    const raw = document.getElementById('testcases-data')?.textContent
    entries = JSON.parse(raw || '[]')
  } catch (_e) {
    entries = []
  }

  if (!entries.length) {
    container.textContent = 'No test cases.'
    return
  }

  const guid = getReportGuid()
  const cards = entries.map((entry, idx) => {
    const colorClass = statusClass(entry)
    const badgeText = statusBadge(entry)
    const debugLink = entry.tceDebugLog
      ? `/debug/${guid}?log=${encodeURIComponent(entry.tceDebugLog)}`
      : ''
    const statsBlock = entry.tceStats
      ? `<h4 class="mt-2 text-[var(--c-grey)]">/* stats */</h4><pre class="bg-[var(--c-dark-grey)] p-3 rounded">${escapeHtml(entry.tceStats)}</pre>`
      : ''
    return `
      <details class="bg-[var(--c-dark-grey)] mb-2 rounded-lg overflow-hidden" id="testcase-${idx + 1}">
        <summary class="flex justify-between items-center cursor-pointer px-4 py-2 hover:bg-[var(--c-grey)] hover:text-[var(--c-black)]">
          <span class="font-mono">${escapeHtml(entry.tceName)}</span>
          <span class="px-2 py-1 ml-2 rounded ${colorClass}">${badgeText}</span>
        </summary>
        <div class="p-4 bg-[var(--c-black)]">
          <h4 class="text-[var(--c-grey)]">/* status */${
            debugLink
              ? `<span class="mt-2 mb-2 ml-2"><a class="outline-link text-[var(--c-blue)]" style="--link-color: var(--c-blue);" href="${debugLink}">[debug]</a></span>`
              : ''
          }</h4>
          <pre class="bg-[var(--c-dark-grey)] p-3 rounded">${escapeHtml(entry.tceStatus)}</pre>
          
          ${statsBlock}
          <h4 class="mt-2 text-[var(--c-grey)]">/* report */</h4>
          <pre class="bg-[var(--c-dark-grey)] p-3 rounded overflow-x-auto">${escapeHtml(entry.tceLog)}</pre>
        </div>
      </details>
    `
  })

  container.innerHTML = cards.join('\n')
}

export const renderReport = () => {
  buildCodeBlock(document.getElementById('assembler-code-text-element'))
  buildCodeBlock(document.getElementById('simulation-config-text-element'))
  buildCodeBlock(document.getElementById('simulation-log-text-element'))
  buildCodeBlock(document.getElementById('dump-text-element'))

  renderDebugNav()
  renderTestCaseCards()
}
