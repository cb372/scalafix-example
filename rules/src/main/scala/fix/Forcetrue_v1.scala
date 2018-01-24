package fix

import scalafix._
import scala.meta._

final case class Forcetrue_v1(index: SemanticdbIndex)
    extends SemanticRule(index, "Forcetrue_v1") {

  override def fix(ctx: RuleCtx): Patch = {
    val patches = ctx.tree.collect {
      case t @
        Term.ApplyInfix(
          Term.ApplyInfix(
            Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Int(1))),
            Term.Name("&&"),
            Nil,
            List(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Int(2))))
          ),
          Term.Name("&&"),
          Nil,
          List(Term.ApplyInfix(Term.Name("a"), Term.Name("=="), Nil, List(Lit.Int(3))))) =>
        ctx.removeTokens(t.tokens) + ctx.addRight(t, "true")
    }
    patches.asPatch
  }

}
