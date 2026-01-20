const escapeHtml = text =>
  text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')

const unescapeHtml = text =>
  text
    // decode double-escaped numeric entities first
    .replace(/&amp;#x([0-9a-fA-F]+);/g, (_m, hex) =>
      String.fromCharCode(parseInt(hex, 16)),
    )
    .replace(/&amp;#(\d+);/g, (_m, dec) =>
      String.fromCharCode(parseInt(dec, 10)),
    )
    // then regular entities
    .replace(/&#x([0-9a-fA-F]+);/g, (_m, hex) =>
      String.fromCharCode(parseInt(hex, 16)),
    )
    .replace(/&#(\d+);/g, (_m, dec) => String.fromCharCode(parseInt(dec, 10)))
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&quot;/g, '"')
    .replace(/&apos;/g, "'")
    .replace(/&#39;/g, "'")
    .replace(/&amp;/g, '&')

const wrapComment = (text, cls) =>
  `<span class="${cls}">${escapeHtml(text)}</span>`

const ISA_RULES = {
  'risc-iv-32': {
    comments: [';'],
    registerPattern: /\b(?:x[0-9]+|[tasfg][0-9]+|ra|sp|gp|tp|fp|zero)\b/gi,
    mnemonics: [
      'lui',
      'mv',
      'sw',
      'sb',
      'lw',
      'addi',
      'add',
      'sub',
      'mul',
      'mulh',
      'div',
      'rem',
      'sll',
      'srl',
      'sra',
      'and',
      'or',
      'xor',
      'j',
      'jal',
      'jr',
      'beqz',
      'bnez',
      'bgt',
      'ble',
      'bgtu',
      'bleu',
      'beq',
      'bne',
      'halt',
    ],
  },
  'vliw-iv': {
    comments: [';'],
    registerPattern: /\b(?:x[0-9]+|[tasfg][0-9]+|ra|sp|gp|tp|fp|zero)\b/gi,
    mnemonics: [
      'lui',
      'mv',
      'addi',
      'add',
      'sub',
      'mul',
      'mulh',
      'div',
      'rem',
      'sll',
      'srl',
      'sra',
      'and',
      'or',
      'xor',
      'slti',
      'nop',
      'lw',
      'sw',
      'sb',
      'j',
      'jal',
      'jr',
      'beqz',
      'bnez',
      'bgt',
      'ble',
      'bgtu',
      'bleu',
      'beq',
      'bne',
      'blt',
      'halt',
    ],
  },
  f32a: {
    comments: ['\\'],
    registerPattern: /\b(?:r[0-9]+|sp|rp)\b/gi,
    mnemonics: [
      'lit',
      '@p',
      '@+',
      '@b',
      '@',
      '!p',
      '!',
      '!+',
      '!b',
      'a!',
      'b!',
      'a',
      '+',
      '+*',
      '+/',
      '2*',
      '2/',
      'inv',
      'eam',
      'and',
      'xor',
      'drop',
      'dup',
      'over',
      ';',
      'next',
      'if',
      '-if',
      'halt',
      'r>',
      '>r',
    ],
  },
  acc32: {
    comments: [';'],
    registerPattern: /\b(?:acc|Acc|ACC|C|V)\b/g,
    mnemonics: [
      'load_imm',
      'load',
      'store',
      'load_addr',
      'store_addr',
      'load_acc',
      'store_ind',
      'add',
      'sub',
      'mul',
      'div',
      'rem',
      'clv',
      'shiftl',
      'shiftr',
      'and',
      'or',
      'xor',
      'not',
      'jmp',
      'beqz',
      'bnez',
      'bgt',
      'ble',
      'bvs',
      'bvc',
      'bcs',
      'bcc',
      'halt',
    ],
  },
  m68k: {
    comments: [';'],
    registerPattern: /\b(?:[DA][0-7]|SP|SR)\b/gi,
    mnemonics: [
      'move.l',
      'move.b',
      'movea.l',
      'not.l',
      'not.b',
      'and.l',
      'and.b',
      'or.l',
      'or.b',
      'xor.l',
      'xor.b',
      'add.l',
      'add.b',
      'sub.l',
      'sub.b',
      'cmp.l',
      'cmp.b',
      'mul.l',
      'mul.b',
      'div.l',
      'div.b',
      'asl.l',
      'asl.b',
      'asr.l',
      'asr.b',
      'lsl.l',
      'lsl.b',
      'lsr.l',
      'lsr.b',
      'jmp',
      'bcc',
      'bcs',
      'beq',
      'bne',
      'blt',
      'bgt',
      'ble',
      'bge',
      'bmi',
      'bpl',
      'bvc',
      'bvs',
      'jsr',
      'rts',
      'link',
      'unlk',
      'halt',
    ],
  },
}

const lower = s => s?.toLowerCase() ?? ''

const buildMnemonicRegex = tokens => {
  if (!tokens || !tokens.length) return null
  const parts = [...tokens]
    .sort((a, b) => b.length - a.length)
    .map(tok => {
      const escaped = tok.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
      const wordish = /^[A-Za-z0-9_.]+$/.test(tok)
      return wordish ? `\\b${escaped}\\b` : escaped
    })
  return new RegExp(`(${parts.join('|')})`, 'gi')
}

const highlightAsmLine = (line, isa) => {
  if (!line) return ''

  const rule = ISA_RULES[lower(isa)] ?? {
    comments: [';', '\\'],
    registerPattern: /\br[0-9]+\b/gi,
    mnemonics: [],
  }

  const commentMarkers =
    rule.comments && rule.comments.length ? rule.comments : [';', '\\']

  const findComment = (text, markers) => {
    let inStr = null
    for (let i = 0; i < text.length; i++) {
      const ch = text[i]
      if (inStr) {
        if (ch === '\\') {
          i += 1 // skip escaped char
          continue
        }
        if (ch === inStr) inStr = null
        continue
      }
      if (ch === '"' || ch === "'") {
        inStr = ch
        continue
      }
      for (const marker of markers) {
        if (text.startsWith(marker, i)) return i
      }
    }
    return -1
  }

  const commentIdx = findComment(line, commentMarkers)

  const comment = commentIdx >= 0 ? line.slice(commentIdx) : ''
  const body = commentIdx >= 0 ? line.slice(0, commentIdx) : line

  // Keep string literals intact so they don't get broken by token highlighting.
  const segments = []
  let current = ''
  let inStr = null
  for (let i = 0; i < body.length; i++) {
    const ch = body[i]
    if (!inStr && (ch === '"' || ch === "'")) {
      if (current) segments.push({ text: current, isString: false })
      inStr = ch
      current = ch
    } else if (inStr) {
      current += ch
      if (ch === inStr && body[i - 1] !== '\\') {
        segments.push({ text: current, isString: true })
        current = ''
        inStr = null
      }
    } else {
      current += ch
    }
  }
  if (current) segments.push({ text: current, isString: !!inStr })

  const processCode = code => {
    const mnemoRe = buildMnemonicRegex(rule.mnemonics)
    const patterns = [
      {
        priority: 1,
        re: /^\s*[A-Za-z_.][\w.]*:/g,
        wrap: m => `<span class="asm-label">${escapeHtml(m)}</span>`,
      },
      {
        priority: 2,
        re: mnemoRe,
        wrap: m => `<span class="asm-mnemonic">${escapeHtml(m)}</span>`,
      },
      {
        priority: 3,
        re: rule.registerPattern,
        wrap: m => `<span class="asm-reg">${escapeHtml(m)}</span>`,
      },
      {
        priority: 4,
        re: /\b0x[0-9a-fA-F_]+\b/g,
        wrap: m => `<span class="asm-number">${escapeHtml(m)}</span>`,
      },
      {
        priority: 5,
        re: /\b-?\d+\b/g,
        wrap: m => `<span class="asm-number">${escapeHtml(m)}</span>`,
      },
    ].filter(p => p.re)

    const matches = []
    for (const { re, wrap, priority } of patterns) {
      re.lastIndex = 0
      let m
      while ((m = re.exec(code)) !== null) {
        matches.push({
          start: m.index,
          end: m.index + m[0].length,
          wrap,
          text: m[0],
          priority,
        })
        if (m.index === re.lastIndex) re.lastIndex++ // avoid zero-length loops
      }
    }

    matches.sort((a, b) => {
      if (a.start !== b.start) return a.start - b.start
      if (a.priority !== b.priority) return a.priority - b.priority
      return a.end - b.end
    })

    const chosen = []
    let lastEnd = -1
    for (const m of matches) {
      if (m.start < lastEnd) continue
      chosen.push(m)
      lastEnd = m.end
    }

    let out = ''
    let cursor = 0
    for (const m of chosen) {
      out += escapeHtml(code.slice(cursor, m.start))
      out += m.wrap(m.text)
      cursor = m.end
    }
    out += escapeHtml(code.slice(cursor))
    return out
  }

  let out = segments
    .map(seg =>
      seg.isString
        ? `<span class="asm-string">${escapeHtml(seg.text)}</span>`
        : processCode(seg.text),
    )
    .join('')

  if (comment) {
    out += wrapComment(comment, 'asm-comment')
  }

  return out
}

const highlightYamlLine = line => {
  if (!line) return ''
  const commentIdx = line.indexOf('#')
  const body = commentIdx >= 0 ? line.slice(0, commentIdx) : line
  const comment = commentIdx >= 0 ? line.slice(commentIdx) : ''

  let out = ''

  const keyRegex = /^(\s*-?\s*)([A-Za-z0-9_.-]+)(\s*:)/ // supports list items like "- key:"
  const keyMatch = body.match(keyRegex)
  let rest = body
  if (keyMatch) {
    const [, pre, key, colon] = keyMatch
    out += escapeHtml(pre)
    out += `<span class="yaml-key">${escapeHtml(key)}</span>${escapeHtml(colon)}`
    rest = body.slice(keyMatch[0].length)
  }

  const tokenRegex = /("[^"]*"|'[^']*'|-?\b\d+(?:\.\d+)?\b)/g
  let lastIndex = 0
  let m
  while ((m = tokenRegex.exec(rest)) !== null) {
    const [match] = m
    const idx = m.index
    out += escapeHtml(rest.slice(lastIndex, idx))
    if (match.startsWith('"') || match.startsWith("'")) {
      out += `<span class="yaml-string">${escapeHtml(match)}</span>`
    } else {
      out += `<span class="yaml-number">${escapeHtml(match)}</span>`
    }
    lastIndex = idx + match.length
  }
  out += escapeHtml(rest.slice(lastIndex))

  if (comment) {
    out += wrapComment(comment, 'yaml-comment')
  }

  return out
}

const highlightBlock = (elementId, lang, isa) => {
  const el = document.getElementById(elementId)
  if (!el) return
  const lines = el.querySelectorAll('.code-line')
  lines.forEach(line => {
    const text = unescapeHtml(line.textContent ?? '')
    line.innerHTML =
      lang === 'yaml' ? highlightYamlLine(text) : highlightAsmLine(text, isa)
  })
}

export const highlightReports = isa => {
  highlightBlock('assembler-code-text-element', 'asm', isa)
  highlightBlock('simulation-config-text-element', 'yaml', isa)
}
