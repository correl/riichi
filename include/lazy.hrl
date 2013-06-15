
-define(LAZY(Expr), fun() ->
                            Expr
                    end).

-define(FORCE(Expr), apply(Expr, [])).

