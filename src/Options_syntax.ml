
open Migrate_parsetree
open Ast_403
open Parsetree
open Location


type 'a expr = Migrate_parsetree.Ast_403.Parsetree.expression


let default_mapper = Ast_mapper.default_mapper
let loc = !Ast_helper.default_loc

let (or) option_expr default_expr =
  [%expr Option.or_else (fun () -> [%e default_expr]) [%e option_expr]]


let mapper _config _cookies =
  let rec map_expr mapper expr =
    match expr.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt=Lident "or"}}, [(Nolabel, opt); (Nolabel, def)]) ->
      (map_expr mapper opt) or (map_expr mapper def)

    | _other ->
      Ast_mapper.default_mapper.expr mapper expr in
  { Ast_mapper.default_mapper with expr = map_expr }


let () =
  Driver.register ~name:"ppx_graphql"
    Versions.ocaml_403
    mapper

