import { highlightReports } from '/assets/code-highlight.js'

const parseJson = id => {
  const el = document.getElementById(id)
  if (!el) return null
  try {
    return JSON.parse(el.textContent || 'null')
  } catch (_e) {
    return null
  }
}

const readAsm = () => {
  const el = document.getElementById('asm-data')
  return (el?.textContent || '').replace(/\r\n/g, '\n')
}

const cloneState = st => ({
  pc: st.esPc,
  registers: { ...(st.esRegisters || st.registers || {}) },
  stacks: Object.fromEntries(
    Object.entries(st.esStacks || st.stacks || {}).map(([k, v]) => [k, [...v]]),
  ),
  memory: { ...(st.esMemory || st.memory || {}) },
  memorySize: st.esMemorySize ?? st.memorySize ?? null,
  memoryProgram: (() => {
    const raw = st.esMemoryProgram || st.memoryProgram || []
    if (!Array.isArray(raw)) return []
    return raw
      .map(range => [
        Number(range?.[0] ?? range?.start ?? 0),
        Number(range?.[1] ?? range?.end ?? 0),
      ])
      .filter(([start, end]) => Number.isFinite(start) && Number.isFinite(end))
      .map(([start, end]) => (start <= end ? [start, end] : [end, start]))
      .sort((a, b) => a[0] - b[0])
  })(),
  io: (() => {
    const raw = st.esIo || st.io || []
    const entries = Array.isArray(raw)
      ? raw.map(({ iosAddr, iosInput = [], iosOutput = [] }) => [
          iosAddr,
          { input: [...iosInput], output: [...iosOutput] },
        ])
      : Object.entries(raw).map(([addr, { input = [], output = [] }]) => [
          Number(addr),
          { input: [...input], output: [...output] },
        ])
    return Object.fromEntries(entries)
  })(),
})

const applyStep = (prev, step, regMemo) => {
  const next = cloneState(prev)
  const memo = regMemo ?? new Map()

  // Backfill missing registers with rcBefore where possible so we keep continuity.
  ;(step.seRegisters || []).forEach(({ rcName, rcBefore }) => {
    if (
      next.registers[rcName] === undefined &&
      rcBefore !== undefined &&
      rcBefore !== null
    ) {
      next.registers[rcName] = rcBefore
      memo.set(rcName, rcBefore)
    }
  })

  if (step.seNextPc !== undefined && step.seNextPc !== null) {
    next.pc = step.seNextPc
  } else if (step.sePc !== undefined && step.sePc !== null) {
    next.pc = step.sePc
  }

  ;(step.seRegisters || []).forEach(({ rcName, rcAfter, rcBefore }) => {
    const val =
      rcAfter ?? memo.get(rcName) ?? rcBefore ?? next.registers[rcName]
    if (val !== undefined) {
      next.registers[rcName] = val
      memo.set(rcName, val)
    }
  })
  ;(step.seStacks || []).forEach(({ scName, scAfter }) => {
    next.stacks[scName] = [...scAfter]
  })
  ;(step.seMemory || []).forEach(({ mcAddr, mcAfter }) => {
    if (mcAfter === null || mcAfter === undefined) delete next.memory[mcAddr]
    else next.memory[mcAddr] = mcAfter
  })
  ;(step.seIo || []).forEach(({ icAddr, icConsumed = [], icProduced = [] }) => {
    const cur = next.io[icAddr] || { input: [], output: [] }
    cur.input = cur.input.slice(icConsumed.length)
    cur.output = [...icProduced, ...cur.output]
    next.io[icAddr] = cur
  })
  return next
}

const fillRegisterTimeline = (states, regNames) => {
  const last = {}
  states.forEach(st => {
    regNames.forEach(name => {
      if (st.registers[name] === undefined && last[name] !== undefined) {
        st.registers[name] = last[name]
      }
    })
    Object.entries(st.registers || {}).forEach(([k, v]) => {
      last[k] = v
    })
  })
}

const buildStates = log => {
  if (!log?.elInitial) return { states: [], steps: [] }
  const steps = log.elSteps || []
  const initial = cloneState(log.elInitial)
  const states = [initial]
  const regMemo = new Map(Object.entries(initial.registers || {}))
  steps.forEach(step => {
    // Ensure the "before" values are remembered even if not present in the snapshot.
    const prev = states[states.length - 1]
    ;(step.seRegisters || []).forEach(({ rcName, rcBefore }) => {
      if (
        prev.registers[rcName] === undefined &&
        rcBefore !== undefined &&
        rcBefore !== null
      ) {
        prev.registers[rcName] = rcBefore
        regMemo.set(rcName, rcBefore)
      }
    })
    states.push(applyStep(prev, step, regMemo))
  })
  return { states, steps }
}

const buildCodeView = asmLines => {
  const container = document.getElementById('assembler-code-text-element')
  if (!container) return
  container.innerHTML = ''

  const wrap = document.createElement('div')
  wrap.className = 'code-container block'

  const scroller = document.createElement('div')
  scroller.className = 'code-scroll'

  const lineNums = document.createElement('div')
  lineNums.className = 'line-numbers'
  const codeContent = document.createElement('div')
  codeContent.className = 'code-content'

  asmLines.forEach((line, idx) => {
    const ln = document.createElement('div')
    ln.className = 'line-number'
    ln.textContent = idx + 1
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

const fmtCharByte = n => {
  if (n === 10) return '\\n'
  if (n === 0) return '\\0'
  if (n >= 32 && n <= 126) return String.fromCharCode(n)
  return `\\x${n.toString(16).toUpperCase().padStart(2, '0')}`
}

const bytesToString = (xs = []) => xs.map(v => fmtCharByte(Number(v))).join('')

const fmtVal = (v, mode) => {
  if (v === undefined || v === null) return '—'
  const n = Number(v)
  if (Number.isNaN(n)) return String(v)
  if (mode === 'char') return fmtCharByte(n)
  if (mode === 'dec') return String(n)
  const toUintHex = num => {
    const hex = (num >>> 0).toString(16).toUpperCase()
    const width = Math.max(2, Math.ceil(hex.length / 2) * 2) // pad to full bytes
    return '0x' + hex.padStart(width, '0')
  }
  if (n < 0) return toUintHex(n)
  const hex = n.toString(16).toUpperCase()
  return '0x' + hex.padStart(Math.max(2, Math.ceil(hex.length / 2) * 2), '0')
}

const SPECIAL_REGS = {
  TOS_D: '__TOS_D__',
  TOS_R: '__TOS_R__',
  EMPTY: '__EMPTY__',
}

const getRegisterLayout = isa => {
  const key = (isa || '').toLowerCase()
  if (key.includes('risc-iv') || key.includes('vliw-iv')) {
    return [
      ['A0', 'S0Fp'],
      ['A1', 'S1'],
      ['A2', 'S2'],
      ['A3', 'S3'],
      ['A4', 'S4'],
      ['A5', 'S5'],
      ['A6', 'S6'],
      ['A7', 'S7'],
      ['T0', 'S8'],
      ['T1', 'S9'],
      ['T2', 'S10'],
      ['T3', 'S11'],
      ['T4', 'Sp'],
      ['T5', 'Ra'],
      ['T6', 'Gp'],
      ['Tp', 'Zero'],
    ]
  }
  if (key.includes('m68k')) {
    return [
      ['A0', 'D0'],
      ['A1', 'D1'],
      ['A2', 'D2'],
      ['A3', 'D3'],
      ['A4', 'D4'],
      ['A5', 'D5'],
      ['A6', 'D6'],
      ['A7', 'D7'],
      ['N', 'Z'],
      ['V', 'C'],
    ]
  }
  if (key.includes('acc32')) {
    return [
      ['ACC', SPECIAL_REGS.EMPTY],
      ['V', 'C'],
    ]
  }
  if (key.includes('f32a')) {
    return [
      ['A', 'B'],
      [SPECIAL_REGS.TOS_D, SPECIAL_REGS.TOS_R],
    ]
  }
  return null
}

const getStackTop = (state, key) => {
  const stack = state?.stacks?.[key] || []
  return stack.length ? stack[0] : undefined
}

const renderRegisters = (state, prevState, changed, mode, allRegs, isa) => {
  const container = document.getElementById('dbg-registers')
  if (!container) return
  const layout = getRegisterLayout(isa)
  if (layout) {
    const renderCell = (name, value, isChanged) => {
      if (name === SPECIAL_REGS.EMPTY) {
        return `<div></div>`
      }
      const label =
        name === SPECIAL_REGS.TOS_D
          ? 'TOS(D)'
          : name === SPECIAL_REGS.TOS_R
            ? 'TOS(R)'
            : name
      const cls = isChanged ? 'changed' : ''
      const display = value === undefined ? '—' : fmtVal(Number(value), mode)
      return `<div class="${cls}">${label}: ${display}</div>`
    }

    const rows = layout.flatMap(([left, right]) => {
      const getVal = name => {
        if (name === SPECIAL_REGS.TOS_D) return getStackTop(state, 'data')
        if (name === SPECIAL_REGS.TOS_R) return getStackTop(state, 'return')
        if (name === SPECIAL_REGS.EMPTY) return undefined
        return state.registers?.[name]
      }
      const getPrev = name => {
        if (name === SPECIAL_REGS.TOS_D) return getStackTop(prevState, 'data')
        if (name === SPECIAL_REGS.TOS_R) return getStackTop(prevState, 'return')
        if (name === SPECIAL_REGS.EMPTY) return undefined
        return prevState?.registers?.[name]
      }

      const leftVal = getVal(left)
      const rightVal = getVal(right)
      const leftChanged =
        left === SPECIAL_REGS.TOS_D || left === SPECIAL_REGS.TOS_R
          ? leftVal !== getPrev(left)
          : changed.has(left)
      const rightChanged =
        right === SPECIAL_REGS.TOS_D || right === SPECIAL_REGS.TOS_R
          ? rightVal !== getPrev(right)
          : changed.has(right)

      return [
        renderCell(left, leftVal, leftChanged),
        renderCell(right, rightVal, rightChanged),
      ]
    })

    container.innerHTML = rows.join('')
    return
  }

  const regs =
    allRegs && allRegs.size
      ? Array.from(allRegs)
      : Object.keys(state.registers || {})
  regs.sort((a, b) => a.localeCompare(b))
  container.innerHTML = regs
    .map(name => {
      const val = state.registers?.[name]
      const prevVal = prevState?.registers?.[name]
      const cls = changed.has(name) ? 'changed' : ''
      const display = val === undefined ? '—' : fmtVal(Number(val), mode)
      // const prevDisplay =
      //   prevVal === undefined || prevVal === val ? '' : ` <span class="text-[var(--c-grey)] text-xs">(${fmtVal(Number(prevVal), mode)} →)</span>`
      return `<div class="${cls}">${name}: ${display}</div>`
    })
    .join('')
}

const renderStacks = (state, changed, mode) => {
  const container = document.getElementById('dbg-stacks')
  if (!container) return
  const entries = Object.entries(state.stacks || {})
  if (!entries.length) {
    container.textContent = 'no stacks'
    return
  }
  entries.sort(([a], [b]) => a.localeCompare(b))
  container.innerHTML = entries
    .map(([name, vals]) => {
      const cls = changed.has(name) ? 'changed' : ''
      const body = (vals || []).map(v => fmtVal(Number(v), mode)).join(', ')
      return `<div class="${cls}">${name}: [${body}]</div>`
    })
    .join('')
}

const renderMemory = (state, changed, wordMode, onlyChanged) => {
  const container = document.getElementById('dbg-memory')
  const counter = document.getElementById('dbg-memory-count')
  if (!container) return
  const entries = Object.entries(state.memory || {}).map(([addr, v]) => [
    Number(addr),
    Number(v),
  ])
  const memMap = new Map(entries)
  const memorySize =
    state.memorySize !== null && state.memorySize !== undefined
      ? Number(state.memorySize)
      : null
  const sparse = memorySize !== null && !Number.isNaN(memorySize)
  const programRanges = state.memoryProgram || []
  const isProgramAddr = addr => {
    for (const [start, end] of programRanges) {
      if (addr < start) return false
      if (addr <= end) return true
    }
    return false
  }
  let blockAddrs = []
  let shownBytes = 0

  if (sparse && !onlyChanged) {
    const size = Math.max(0, memorySize)
    const lastBase = size > 0 ? Math.floor((size - 1) / 4) * 4 : -1
    for (let base = 0; base <= lastBase; base += 4) {
      blockAddrs.push(base)
    }
    shownBytes = size
  } else {
    const displayAddrs = (
      onlyChanged ? Array.from(changed) : entries.map(([addr]) => addr)
    ).sort((a, b) => a - b)

    const blocks = new Set()
    displayAddrs.forEach(addr => {
      const base = Math.floor(addr / 4) * 4
      blocks.add(base)
    })

    blockAddrs = Array.from(blocks.keys()).sort((a, b) => a - b)
    shownBytes = displayAddrs.length
  }
  const formatByte = v =>
    v === undefined || v === null
      ? '??'
      : Number(v).toString(16).toUpperCase().padStart(2, '0')
  const getByte = addr => {
    if (isProgramAddr(addr)) return undefined
    if (memMap.has(addr)) return memMap.get(addr)
    if (!sparse) return undefined
    if (addr < 0 || addr >= memorySize) return undefined
    return 0
  }
  const rows = blockAddrs.map(base => {
    const bytes = [
      getByte(base),
      getByte(base + 1),
      getByte(base + 2),
      getByte(base + 3),
    ]
    const byteCells = bytes.map((b, i) => {
      const addr = base + i
      const classes = []
      if (changed.has(addr)) classes.push('changed')
      if (isProgramAddr(addr)) classes.push('program-cell')
      const cls = classes.join(' ')
      return `<span class="${cls} mr-1">${formatByte(b)}</span>`
    })
    // If any byte missing, skip summary to avoid wrong value.
    const complete = bytes.every(b => b !== undefined && b !== null)
    let word = '—'
    if (complete) {
      if (wordMode === 'char') {
        word = `"${bytesToString(bytes)}"`
      } else {
        let acc = 0
        for (let i = 0; i < 4; i++) {
          acc |= (bytes[i] & 0xff) << (8 * i) // little-endian
        }
        word = fmtVal(acc, wordMode)
      }
    }
    const addrLabel = `0x${base.toString(16).toUpperCase().padStart(4, '0')}`
    const changedCls = bytes.some((_, i) => changed.has(base + i))
      ? 'changed-row'
      : ''
    const programCls = bytes.some((_, i) => isProgramAddr(base + i))
      ? 'program-row'
      : ''
    const rowCls = [changedCls, programCls].filter(Boolean).join(' ')
    return `<div class="flex items-center justify-between gap-2 ${rowCls}" data-base="${base}">
      <span class="text-[var(--c-grey)]">${addrLabel}</span>
      <div class="flex-1 flex items-center gap-1">${byteCells.join('')}</div>
      <span class="text-[var(--c-grey)]">${word}</span>
    </div>`
  })

  container.innerHTML =
    rows.join('') || '<div class="text-[var(--c-grey)]">memory empty</div>'
  if (counter) {
    const total = sparse ? memorySize : entries.length
    counter.textContent = `${shownBytes} bytes shown / ${total} total`
  }

  // Scroll first changed row into view.
  const firstChanged = container.querySelector('.changed-row')
  if (firstChanged && container.scrollHeight > container.clientHeight) {
    const top = firstChanged.offsetTop
    const bottom = top + firstChanged.offsetHeight
    const viewTop = container.scrollTop
    const viewBottom = viewTop + container.clientHeight
    if (top < viewTop || bottom > viewBottom) {
      container.scrollTop = Math.max(
        0,
        top - container.offsetTop - container.offsetHeight / 2,
      )
    }
  }
}

const renderIo = (state, step, mode) => {
  const container = document.getElementById('dbg-io')
  if (!container) return
  const entries = Object.entries(state.io || {}).map(([addr, io]) => [
    Number(addr),
    io,
  ])
  entries.sort((a, b) => a[0] - b[0])
  const consumedMap = new Map()
  const producedMap = new Map()
  ;(step?.seIo || []).forEach(
    ({ icAddr, icConsumed = [], icProduced = [] }) => {
      consumedMap.set(icAddr, icConsumed)
      producedMap.set(icAddr, icProduced)
    },
  )
  container.innerHTML = entries
    .map(([addr, { input = [], output = [] }]) => {
      const consumed = consumedMap.get(addr) || []
      const produced = producedMap.get(addr) || []
      const emptyMarker =
        '<span class="text-[var(--c-grey)] italic">*Empty*</span>'
      const fmtList = (xs, extra = []) => {
        if (!xs.length && !extra.length) return emptyMarker
        if (mode === 'char') {
          const base = bytesToString(xs)
          const extraStr = extra.length ? bytesToString(extra) : ''
          return `${base}${extraStr ? ` <span class="changed">(-${extraStr})</span>` : ''}`
        }
        const body = xs.map(v => fmtVal(Number(v), mode))
        if (extra.length) {
          const extraFmt = extra.map(v => fmtVal(Number(v), mode)).join(', ')
          return `${body.join(', ')} <span class="changed">(-${extraFmt})</span>`
        }
        return body.join(', ')
      }
      const producedFmt =
        produced.length && mode === 'char'
          ? `<span class="changed">(+${bytesToString(produced)})</span>`
          : produced.length
            ? `<span class="changed">(+${produced.map(v => fmtVal(Number(v), mode)).join(', ')})</span>`
            : ''
      const outBody =
        output.length
          ? mode === 'char'
            ? bytesToString([...output].reverse())
            : [...output]
                .reverse()
                .map(v => fmtVal(Number(v), mode))
                .join(', ')
          : ''
      return `<div class="mb-2">
        <div class="text-[var(--c-grey)]">addr 0x${addr.toString(16)}</div>
        <div>in: ${fmtList(input, consumed)}</div>
        <div>out: ${outBody || (!produced.length ? emptyMarker : '')} ${producedFmt}</div>
      </div>`
    })
    .join('')
}

const highlightCode = line => {
  const lines = document.querySelectorAll(
    '#assembler-code-text-element .code-line',
  )
  lines.forEach(ln => ln.classList.remove('active-line'))
  if (!line) return
  const target = document.querySelector(
    `#assembler-code-text-element .code-line[data-line="${line}"]`,
  )
  if (target) {
    target.classList.add('active-line')
    const scroller = document.querySelector(
      '#assembler-code-text-element .code-scroll',
    )
    const content = document.querySelector(
      '#assembler-code-text-element .code-content',
    )
    if (scroller) {
      const top = target.offsetTop
      const bottom = top + target.offsetHeight
      const viewTop = scroller.scrollTop
      const viewBottom = viewTop + scroller.clientHeight
      if (top < viewTop || bottom > viewBottom) {
        scroller.scrollTop = top - scroller.clientHeight / 3
      }
      if (content) {
        const left = target.offsetLeft
        const right = left + target.offsetWidth
        const viewLeft = content.scrollLeft
        const viewRight = viewLeft + content.clientWidth
        if (left < viewLeft || right > viewRight) {
          content.scrollLeft = left - content.clientWidth / 3
        }
      }
    } else {
      target.scrollIntoView({ block: 'center', inline: 'nearest' })
    }
  }
}

const setupDebugger = () => {
  const log = parseJson('exec-log-data')
  const asmText = readAsm()
  const isaEl = document.getElementById('dbg-isa')
  const isa = isaEl?.textContent?.trim().toLowerCase() || ''
  const errorEl = document.getElementById('debug-error')
  if (!log || !log.elInitial) {
    errorEl?.classList.remove('hidden')
    return
  }
  const asmLines = asmText.split('\n')
  buildCodeView(asmLines)
  highlightReports(isa)

  const { states, steps } = buildStates(log)
  const allRegs = new Set([
    ...Object.keys(log.elInitial?.esRegisters || {}),
    ...steps.flatMap(s => (s.seRegisters || []).map(r => r.rcName)),
  ])
  fillRegisterTimeline(states, allRegs)

  const pcToLine = new Map()
  steps.forEach(s => {
    if (s?.seSource?.siLine != null) pcToLine.set(s.sePc, s.seSource.siLine)
  })
  if (!states.length) {
    errorEl?.classList.remove('hidden')
    return
  }

  let idx = 0
  let playing = false
  let timer = null
  const speedEl = document.getElementById('dbg-speed')
  const speedLabel = document.getElementById('dbg-speed-label')
  const valueBaseEl = document.getElementById('dbg-value-base')
  const ioBaseEl = document.getElementById('dbg-io-base')
  const memChangedOnlyEl = document.getElementById('dbg-memory-changed-only')
  const memWordBaseEl = document.getElementById('dbg-memory-word-base')
  const infoEl = document.getElementById('dbg-step-info')
  const instrEl = document.getElementById('dbg-instruction')

  const stop = () => {
    playing = false
    if (timer) clearTimeout(timer)
    timer = null
    const playBtn = document.getElementById('dbg-play')
    if (playBtn) playBtn.textContent = 'play'
  }

  const schedule = () => {
    if (!playing) return
    const speed = parseFloat(speedEl?.value || '1')
    const delay = Math.max(50, 200 / speed)
    timer = setTimeout(() => {
      stepForward()
      schedule()
    }, delay)
  }

  let lastLine = null

  const render = () => {
    const state = states[idx]
    const step = idx > 0 ? steps[idx - 1] : null
    const highlightStep = idx === 0 ? steps[0] : step
    const mappedLine =
      highlightStep?.seSource?.siLine + 1 ??
      (highlightStep ? pcToLine.get(highlightStep.sePc) : null) ??
      lastLine
    const regChanged = new Set(step?.seRegisters?.map(r => r.rcName) || [])
    const stackChanged = new Set(step?.seStacks?.map(s => s.scName) || [])
    const memChanged = new Set((step?.seMemory || []).map(m => m.mcAddr))

    const mode = valueBaseEl?.value || 'hex'
    const ioMode = ioBaseEl?.value || 'dec'
    const onlyChanged = memChangedOnlyEl?.checked || false
    const memWordMode = memWordBaseEl?.value || 'hex'

    const prevState = idx > 0 ? states[idx - 1] : null
    renderRegisters(state, prevState, regChanged, mode, allRegs, isa)
    renderStacks(state, stackChanged, mode)
    renderMemory(state, memChanged, memWordMode, onlyChanged)
    renderIo(state, step, ioMode)

    if (infoEl)
      infoEl.textContent = `step ${idx}/${states.length - 1} | pc 0x${state.pc.toString(16)}`
    if (instrEl) {
      const instrText = step?.seInstruction || ''
      const label = step?.seLabel ? `@${step.seLabel}` : ''
      instrEl.textContent = [label, instrText].filter(Boolean).join(' ')
    }
    if (mappedLine) lastLine = mappedLine
    highlightCode(mappedLine)
  }

  const stepForward = () => {
    if (idx < states.length - 1) {
      idx += 1
      render()
    } else {
      stop()
    }
  }

  const stepBack = () => {
    if (idx > 0) {
      idx -= 1
      render()
    }
  }

  document.getElementById('dbg-reset')?.addEventListener('click', () => {
    stop()
    idx = 0
    render()
  })
  document.getElementById('dbg-prev')?.addEventListener('click', () => {
    stop()
    stepBack()
  })
  document.getElementById('dbg-next')?.addEventListener('click', () => {
    stop()
    stepForward()
  })
  document.getElementById('dbg-play')?.addEventListener('click', () => {
    if (playing) {
      stop()
    } else {
      if (idx >= states.length - 1) {
        idx = 0
        render()
      }
      playing = true
      document.getElementById('dbg-play').textContent = 'pause'
      schedule()
    }
  })
  speedEl?.addEventListener('input', () => {
    speedLabel.textContent = `${parseFloat(speedEl.value).toFixed(2)}x`
    if (playing) {
      stop()
      playing = true
      document.getElementById('dbg-play').textContent = 'pause'
      schedule()
    }
  })
  valueBaseEl?.addEventListener('change', render)
  ioBaseEl?.addEventListener('change', render)
  memWordBaseEl?.addEventListener('change', render)
  memChangedOnlyEl?.addEventListener('change', render)

  render()
}

document.addEventListener('DOMContentLoaded', setupDebugger)
