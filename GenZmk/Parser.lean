import Lean.Data.Parsec

namespace Parser
open Lean Parsec

inductive Sexpr where
  | atom (x : String)      : Sexpr
  | list (xs : List Sexpr) : Sexpr
  deriving Repr

def stringContentP : Parsec String := do
   many1Chars (satisfy (. != '"'))

def stringP : Parsec Sexpr := do
   let _ <- Parsec.pchar '"'
   let s <- stringContentP
   let _ <- Parsec.pchar '"'
   pure $ Sexpr.atom s

#eval Parsec.run stringP "\"a string \""

def atomP : Parsec Sexpr := stringP <|> do
  let atom <- many1Chars (satisfy (fun c => c != ' ' && c != '\n' && c != '(' && c != ')'))
  let eatom <- match atom.toList with
    | ['\'', '"'] => pure "\""
    | ['\''] => do
        let n <- peek!
        if n == '(' || n == ')' then do
          skip
          pure n.toString
        else pure atom
    | _ => pure atom
  pure (Sexpr.atom eatom)

def commentP : Parsec Char := do
  let _ <- pstring ";;"
  let _ <- many (satisfy (. != '\n'))
  pure ' '

def skipP : Parsec Unit := do
  let _ <- many (commentP <|> satisfy (fun c => c == ' ' || c == '\n'))
  pure ()

mutual
partial def exprP : Parsec Sexpr := do
  skipP
  let r <- listP <|> atomP
  skipP
  pure r

partial def listP : Parsec Sexpr := do
  let _ <- pstring "("
  let xs <- many1 exprP
  let _ <- pstring ")"
  pure $ Sexpr.list xs.toList
end

def configP : Parsec (List Sexpr) := do
  let e <- many1 exprP
  Parsec.eof
  pure e.toList

def Sexpr.parse := Parsec.run configP

#eval (Parsec.run configP ";; ma config!
(quoted '\" '( '))
(config :key \"a value\"
  (layout test a b c)) ;; comment
a ;; done")

end Parser
