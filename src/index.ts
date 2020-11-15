class Loc {
  static merge(locs: Loc[]) {
    return locs.reduce((v, loc) => v.merge(loc), new Loc(Infinity, -Infinity))
  }

  start: number

  end: number

  constructor(start: number, end: number) {
    this.start = start
    this.end = end
  }

  merge(other: Loc) {
    return new Loc(
      Math.min(this.start, other.start),
      Math.max(this.end, other.end)
    )
  }
}

class Walker<T> {
  values: T[]

  index = 0

  constructor(values: T[]) {
    this.values = values
  }

  get() {
    return this.values[this.index]
  }

  next() {
    this.index++

    return this.get()
  }

  peek() {
    return this.values[this.index + 1]
  }
}

function loop<T>(callback: (end: (value: T) => void) => void): T {
  let isEnded = false
  let result: T

  const end = (value: T) => {
    isEnded = true
    result = value
  }

  while (!isEnded) {
    callback(end)
  }

  return result
}

type Annot<T extends string> = {
  kind: T

  loc: Loc
}

type TokenBase<T extends string> = Annot<T>

type TokenNum = TokenBase<'number'> & {
  value: number
}

type TokenPlus = TokenBase<'plus'>

type TokenMinus = TokenBase<'minus'>

type TokenAsterisk = TokenBase<'asterisk'>

type TokenSlash = TokenBase<'slash'>

type TokenLeftParen = TokenBase<'left_paren'>

type TokenRightParen = TokenBase<'right_paren'>

type TokenSemi = TokenBase<'semi'>

type TokenEnd = TokenBase<'end'>

type Token =
  | TokenNum
  | TokenPlus
  | TokenMinus
  | TokenAsterisk
  | TokenSlash
  | TokenLeftParen
  | TokenRightParen
  | TokenSemi
  | TokenEnd

function lex(input: string): Token[] {
  const chars = input.split('')
  const tokens: Token[] = []

  const numberExp = /[0-9]/

  for (let i = 0; i < chars.length; i++) {
    const char = chars[i]
    const start = i

    if (numberExp.test(char)) {
      let numberString = ''

      while (numberExp.test(chars[i])) {
        numberString += chars[i]
        i++
      }
      i--

      tokens.push({
        kind: 'number',
        value: Number(numberString),
        loc: new Loc(start, i + 1),
      })
    }

    if (char === '+') {
      tokens.push({
        kind: 'plus',
        loc: new Loc(start, start + 1),
      })
    }

    if (char === '-') {
      tokens.push({
        kind: 'minus',
        loc: new Loc(start, start + 1),
      })
    }

    if (char === '*') {
      tokens.push({
        kind: 'asterisk',
        loc: new Loc(start, start + 1),
      })
    }

    if (char === '/') {
      tokens.push({
        kind: 'slash',
        loc: new Loc(start, start + 1),
      })
    }

    if (char === '(') {
      tokens.push({
        kind: 'left_paren',
        loc: new Loc(start, start + 1),
      })
    }

    if (char === ')') {
      tokens.push({
        kind: 'right_paren',
        loc: new Loc(start, start + 1),
      })
    }

    if (char === ';') {
      tokens.push({
        kind: 'semi',
        loc: new Loc(start, start + 1),
      })
    }
  }

  tokens.push({
    kind: 'end',
    loc: new Loc(0, 0),
  })

  return tokens
}

type NodeBase<T extends string> = Annot<T>

type NodeProgram = NodeBase<'program'> & {
  stmts: NodeStmt[]
}

type NodeSemi = NodeBase<'semi'> & {
  expr: NodeExpr
}

type NodeStmt = NodeSemi

type NodeBinary = NodeBase<'binary'> & {
  opToken: Token

  left: NodeExpr

  right: NodeExpr
}

type NodeNumber = NodeBase<'number'> & {
  value: number
}

type NodeExpr = NodeBinary | NodeNumber

type AstNode = NodeProgram | NodeStmt | NodeExpr

type ParserFn<T extends AstNode> = (walker: Walker<Token>) => T

function kindToSymbol(kind: Token['kind']) {
  const map: {
    [K in Token['kind']]: string
  } = {
    number: 'number',
    plus: '+',
    minus: '-',
    asterisk: '*',
    slash: '/',
    left_paren: '(',
    right_paren: ')',
    semi: ';',
    end: 'end',
  }

  return map[kind]
}

function expectToken(expectKind: Token['kind'], token?: Token) {
  if (!token) {
    throw new Error('peek error')
  }

  if (token.kind === expectKind) {
    return
  }

  throw new Error(
    `expcet ${kindToSymbol(expectKind)}, but got ${kindToSymbol(token.kind)}`
  )
}

function parse(tokens: Token[]): NodeProgram {
  const walker = new Walker(tokens)
  const stmts: NodeStmt[] = []

  loop<void>((end) => {
    stmts.push(parseStmt(walker))

    if (walker.get().kind === 'end') {
      end()
    }
  })

  return {
    kind: 'program',
    stmts,
    loc: Loc.merge(stmts.map(({ loc }) => loc)),
  }
}

const parseStmt: ParserFn<NodeStmt> = (walker) => {
  const expr = parseExpr(walker)
  expectToken('semi', walker.get())

  walker.next()

  return {
    kind: 'semi',
    expr,
    loc: expr.loc,
  }
}

const parseExpr: ParserFn<NodeExpr> = (walker) => {
  return parseAdd(walker)
}

const parseBinary = (
  walker: Walker<Token>,
  subParser: ParserFn<NodeExpr>,
  validateToken: (token: Token) => boolean
): NodeExpr => {
  let left: NodeExpr = subParser(walker)

  return loop((end) => {
    const token = walker.get()

    if (validateToken(token)) {
      walker.next()
      const right = subParser(walker)

      left = {
        kind: 'binary',
        opToken: token,
        left,
        right,
        loc: left.loc.merge(right.loc),
      }
    } else {
      return end(left)
    }
  })
}

const parseAdd: ParserFn<NodeExpr> = (walker) => {
  return parseBinary(
    walker,
    parseMul,
    ({ kind }) => kind === 'plus' || kind === 'minus'
  )
}

const parseMul: ParserFn<NodeExpr> = (walker) => {
  return parseBinary(
    walker,
    parseAtom,
    ({ kind }) => kind === 'asterisk' || kind === 'slash'
  )
}

const parseAtom: ParserFn<NodeExpr> = (walker) => {
  const token = walker.get()

  if (!token) {
    throw new Error('peek error')
  }

  if (token.kind === 'left_paren') {
    walker.next()

    const expr = parseExpr(walker)
    expectToken('right_paren', walker.get())
    walker.next()

    return expr
  }

  if (token.kind === 'number') {
    walker.next()

    return {
      kind: 'number',
      value: token.value,
      loc: token.loc,
    }
  }

  throw new Error(`unexpected token ${token.kind}`)
}

function gen(ast: AstNode): string {
  if (ast.kind === 'program') {
    let code = ''

    for (const stmt of ast.stmts) {
      code += `${gen(stmt)};\n`
    }

    return code
  }

  if (ast.kind === 'semi') {
    return gen(ast.expr)
  }

  if (ast.kind === 'binary') {
    return `(${gen(ast.left)} ${kindToSymbol(ast.opToken.kind)} ${gen(
      ast.right
    )})`
  }

  if (ast.kind === 'number') {
    return ast.value.toString()
  }
}

function main() {
  const source = '(1 + 2) * 3;'

  const tokens = lex(source)
  const ast = parse(tokens)
  eval(gen(ast))
}

main()
