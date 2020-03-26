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

let isTypeInheritUnsafe = function
  | SynType.LongIdent (LongIdentWithDots(ident, _)) ->
    ident |> isLongIdentIncludeUnsafe
  | SynType.LongIdentApp (_, LongIdentWithDots(ident, _), _, _, _, _, _) ->
    ident |> isLongIdentIncludeUnsafe
  | _ -> false

let rec visitExpr = function
  // Application
  | SynExpr.App (_, _, f, arg, _) ->
    visitExpr f || visitExpr arg

  // Long Identifier
  | SynExpr.LongIdent (_, LongIdentWithDots (ident, _), _, _) ->
    ident |> isLongIdentIncludeUnsafe

  // Match expressions
  | SynExpr.Match (_, e, clauses, _)
  | SynExpr.MatchBang (_, e, clauses, _) ->
    visitExpr e
    || clauses |> List.exists visitClause
  | SynExpr.MatchLambda (_, _, clauses, _, _) ->
    clauses |> List.exists visitClause

  // If-Then-Else expression
  | SynExpr.IfThenElse (g, t, f, _, _, _, _) ->
    visitExpr g
    || visitExpr t
    || (match f with | Some f -> visitExpr f | None -> false)

  // Let/Use expression
  | SynExpr.LetOrUse (_, _, bindings, body, _) ->
    bindings |> List.exists visitBinding
    || visitExpr body
  | SynExpr.LetOrUseBang (_, _, _, _, rhs, _, body, _) ->
    visitExpr rhs || visitExpr body

  // Array/List expression
  | SynExpr.ArrayOrList (_, exprs, _) ->
    exprs |> List.exists visitExpr
  | SynExpr.ArrayOrListOfSeqExpr (_, e, _) ->
    visitExpr e

  // Loop expressions
  | SynExpr.ForEach (_, _, _, _, enum, body, _) ->
    visitExpr enum || visitExpr body
  | SynExpr.For (_, _, f, _, t, d, _) ->
    visitExpr f || visitExpr t || visitExpr d
  | SynExpr.While (_, cond, body, _) ->
    visitExpr cond || visitExpr body

  // Sequential, Tuple expression
  | SynExpr.Sequential (_, _, e1, e2, _) ->
    visitExpr e1 || visitExpr e2
  | SynExpr.Tuple (_, exprs, _, _) ->
    exprs |> List.exists visitExpr

  // Exception expressions
  | SynExpr.TryFinally(t, f, _, _, _) ->
    visitExpr t || visitExpr f
  | SynExpr.TryWith(body, _, cases, _, _, _, _) ->
    visitExpr body
    || cases |> List.exists visitClause

  // Get/Set expressions
  | SynExpr.Set (_, e, _)
  | SynExpr.LongIdentSet (_, e, _) ->
    visitExpr e
  | SynExpr.NamedIndexedPropertySet (LongIdentWithDots (_, _), i, e, _) ->
    visitExpr i || visitExpr e
  | SynExpr.DotGet (e, _, _, _) ->
    visitExpr e
  | SynExpr.DotIndexedGet (e, indices, _, _) ->
    visitExpr e
    || indices |> List.exists visitIndex
  | SynExpr.DotSet (p, _, e, _) ->
    visitExpr p || visitExpr e
  | SynExpr.DotIndexedSet (p, indices, e, _, _, _) ->
    visitExpr p || visitExpr e
    || indices |> List.exists visitIndex
  | SynExpr.DotNamedIndexedPropertySet (p, _, i, e, _) ->
    visitExpr p || visitExpr i || visitExpr e

  // Record expressions
  | SynExpr.Record (con, copy, fields, _) ->
    (match con with | Some (_, e, _, _, _) -> visitExpr e | None -> false)
    || (match copy with | Some (e, _) -> visitExpr e | None -> false)
    || fields |> List.exists (function | (_, Some e, _) -> visitExpr e | _ -> false)
  | SynExpr.AnonRecd (_, copy, fields, _) ->
    (match copy with | Some (e, _) -> visitExpr e | None -> false)
    || fields |> List.exists (fun (_, e) -> visitExpr e)

  // Object expression
  | SynExpr.ObjExpr (pType, opt, bindings, impls, _, _) ->
    pType |> isTypeInheritUnsafe
    || (match opt with | Some (e, _) -> visitExpr e | None -> false)
    || bindings |> List.exists visitBinding
    || impls |> List.exists (fun (InterfaceImpl(_, bindings, _)) ->
      bindings |> List.exists visitBinding)

  // Wrapped cases
  | SynExpr.Paren (e, _, _, _)
  | SynExpr.CompExpr (_, _, e, _)
  | SynExpr.Assert (e, _)
  | SynExpr.AddressOf (_, e, _, _)
  | SynExpr.Do (e, _)
  | SynExpr.DoBang (e, _)
  | SynExpr.Downcast (e, _, _)
  | SynExpr.Upcast (e, _, _)
  | SynExpr.InferredDowncast (e, _)
  | SynExpr.InferredUpcast (e, _)
  | SynExpr.Fixed (e, _)
  | SynExpr.Lazy (e, _)
  | SynExpr.New (_, _, e, _)
  | SynExpr.Typed (e, _, _)
  | SynExpr.TypeTest (e, _, _)
  | SynExpr.YieldOrReturn (_, e, _)
  | SynExpr.YieldOrReturnFrom (_, e, _) -> visitExpr e
  | _ -> false

and visitBinding = fun (Binding (_, _, _, _, _, _, _, _, _, e, _, _)) ->
  visitExpr e

and visitClause = fun (Clause (_, wh, e, _, _)) ->
  visitExpr e
  || (match wh with | Some e -> visitExpr e | None -> false)

and visitIndex = function
  | SynIndexerArg.One (index, _, _) ->
    visitExpr index
  | SynIndexerArg.Two (index1, _, index2, _, _, _) ->
    visitExpr index1 || visitExpr index2

let rec sanitizeMembers (defns : SynMemberDefns) =
  defns
  |> List.exists (fun defn ->
    match defn with
      | SynMemberDefn.Open (ident, _) ->
        ident |> isLongIdentIncludeUnsafe
      | SynMemberDefn.Member (binding, _) ->
        visitBinding binding
      | SynMemberDefn.LetBindings (bindings, _, _, _) ->
        bindings |> List.exists visitBinding
      | SynMemberDefn.Interface (_, def, _) ->
        (match def with | Some def -> sanitizeMembers def | None -> false)
      | SynMemberDefn.Inherit (pType, _, _) ->
        pType |> isTypeInheritUnsafe
      | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, e, _, _) ->
        visitExpr e
      | _ -> false
  )

let rec sanitizeDecls (decls: SynModuleDecls) =
  decls
  |> List.exists (fun decl ->
    match decl with
    | SynModuleDecl.DoExpr (_, e, _) -> visitExpr e
    | SynModuleDecl.Let (_, bindings, _) ->
      bindings |> List.exists visitBinding
    | SynModuleDecl.ModuleAbbrev (_, ident, _) ->
      ident |> isLongIdentIncludeUnsafe
    | SynModuleDecl.NestedModule (_, _, decls, _, _) ->
      sanitizeDecls decls
    | SynModuleDecl.Open (LongIdentWithDots (ident, _), _) ->
      ident |> isLongIdentIncludeUnsafe
    | SynModuleDecl.Types (types, _) ->
      types |> List.exists (fun (TypeDefn(_, _, members, _)) ->
        sanitizeMembers members)
    | SynModuleDecl.NamespaceFragment (SynModuleOrNamespace (ident, _, _, decls, _, _, _, _)) ->
      ident |> isLongIdentIncludeUnsafe || sanitizeDecls decls
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