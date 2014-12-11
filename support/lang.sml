structure Lang = 
struct

   exception LangError of string

   open Term
   infixr $$

   fun Let (e, var) e2 = TermOps.Let $$ [e, \\(var, e2)]

   fun Lam (var, tyarg) funend = TermOps.Lam tyarg $$ [\\(var, funend)]

   fun Fix (var, tyarg) funend = TermOps.Fix tyarg $$ [\\(var, funend)]

   fun Case e lcase rcase = TermOps.Case $$ [e, \\(lcase), \\(rcase)]

   fun Abort tyarg e = TermOps.Abort tyarg $$ [e]

   fun Ifz e zifz sifz = TermOps.Ifz $$ [e, zifz, \\(sifz)]
  
   fun PrL e = TermOps.Pr TermOps.L $$ [e]

   fun PrR e = TermOps.Pr TermOps.R $$ [e]

   fun InL (sum1, sum2) e = TermOps.In (sum1, sum2, TermOps.L) $$ [e]

   fun InR (sum1, sum2) e = TermOps.In (sum1, sum2, TermOps.R) $$ [e]

   fun Match _ = raise LangError "Match not present in this semantics"

end
