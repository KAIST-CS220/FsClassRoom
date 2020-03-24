// In-Class Coding Activity for CS220 @ KAIST
// Author: Sang Kil Cha <sangkilc@kaist.ac.kr>

module FsClassroom.Sanitizer

open System.IO
open FSharp.Compiler.Text
open FSharp.Compiler.Ast
open FSharp.Compiler.SourceCodeServices

let unsafe = ["System"; "NativePtr"]

let isLongIdentIncludeUnsafe (ident: LongIdent) =
  ident |> Seq.exists (fun i -> List.contains i.idText unsafe)

let rec visitExpr = function
  | SynExpr.App (_, _, f, arg, _) ->
    visitExpr f
    || visitExpr arg
  | SynExpr.LongIdent (_, LongIdentWithDots (ident, _), _, _) ->
    ident |> isLongIdentIncludeUnsafe
  | SynExpr.Match (_, e, clauses, _) ->
    visitExpr e
    || clauses |> List.fold (fun acc (Clause (_, wh, e, _, _)) ->
      acc
      || visitExpr e
      || (match wh with | Some e -> visitExpr e | None -> false)) false
  | SynExpr.IfThenElse (g, t, f, _, _, _, _) ->
    visitExpr g
    || visitExpr t
    || (match f with | Some f -> visitExpr f | None -> false)
  | SynExpr.LetOrUse (_, _, bindings, body, _) ->
    bindings
    |> List.forall (fun (Binding (_, _, _, _, _, _, _, _, _, e, _, _)) ->
      visitExpr e)
    || visitExpr body
  | SynExpr.LetOrUseBang (_, _, _, _, rhs, _, body, _) ->
    visitExpr rhs || visitExpr body
  | SynExpr.ArrayOrList (_, exprs, _) ->
    exprs |> List.exists visitExpr
  | SynExpr.ArrayOrListOfSeqExpr (_, e, _) ->
    visitExpr e
  | SynExpr.CompExpr (_, _, e, _) ->
    visitExpr e
  | SynExpr.ForEach (_, _, _, _, enum, body, _) ->
    visitExpr enum
    || visitExpr body
  | SynExpr.Sequential (_, _, e1, e2, _) ->
    visitExpr e1
    || visitExpr e2
  | _ -> false

let rec sanitizeDecls (decls: SynModuleDecls) =
  decls
  |> List.exists (fun decl ->
    match decl with
    | SynModuleDecl.DoExpr (_, e, _) -> visitExpr e
    | SynModuleDecl.Let (_, bindings, _) ->
      bindings
      |> List.forall (fun (Binding (_, _, _, _, _, _, _, _, _, e, _, _)) ->
        visitExpr e)
    | SynModuleDecl.ModuleAbbrev (_, ident, _) ->
      ident |> isLongIdentIncludeUnsafe
    | SynModuleDecl.NestedModule (_, _, decls, _, _) ->
      sanitizeDecls decls
    | SynModuleDecl.Open (LongIdentWithDots (ident, _), _) ->
      ident |> isLongIdentIncludeUnsafe
    | _ -> false)

let sanitizeModule = function
  | SynModuleOrNamespace (_, _, _, decls, _, _, _, _) -> sanitizeDecls decls

let sanitize (codepath: string) (checker: FSharpChecker) =
  let input = File.ReadAllText codepath |> SourceText.ofString
  let projOpts, _ =
    checker.GetProjectOptionsFromScript (codepath, input)
    |> Async.RunSynchronously
  let parseOpts, _ = checker.GetParsingOptionsFromProjectOptions (projOpts)
  let results =
    checker.ParseFile (codepath, input, parseOpts) |> Async.RunSynchronously
  match results.ParseTree with
  | Some (ParsedInput.ImplFile (ParsedImplFileInput (_, _, _, _, _, m, _))) ->
    m |> List.exists sanitizeModule
  | _ -> false
