
type expression = Parsetree.expression

val (or) : expression -> expression -> expression
(** Macro for conditional evaluation of default values for option types.

    [option or default] is translated into [Option.or_else (fun () -> default)
    option].

    {b Note:} [or] is a  has a very low precedence, {i e.g.} [option or 0 + 1] is
    evaluated as [option or (0 + 1)]. Complex expressions should be grouped
    explicitly to avoid ambiguity.

    {b Examples:}

{[
assert ((List.head [] or 0) = 0);
assert ((List.head [] or List.head [1] or 0) = 1);
assert ((List.head [] or List.head []  or 0) = 0);
assert ((List.head [] or 1 + 2) = 3);
]} *)
