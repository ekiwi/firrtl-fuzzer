package generators

import firrtl.ir._


case class FirrtlModuleSettings(primOpSettings: FirrtlPrimOpSettings = FirrtlPrimOpSettings(), typeSettings: FirrtlTypeSettings = FirrtlTypeSettings())


class FirrtlModuleGenerator(val settings: FirrtlModuleSettings, val modules: Seq[Module] = Seq()) extends FirrtlGenerator {
  val typeGenerator = new FirrtlTypeGenerator(settings.typeSettings)
  val exprGenerator = new FirrtlExprGenerator(settings.primOpSettings, settings.typeSettings)


  // TODO: for now we only support ground types!
  val GroundTypesOnly = true


  private def generateIO(rand: Random) = {
    val maxDepth = if(GroundTypesOnly) { 1 } else { 4 }
    Port(NoInfo, "io", Input, typeGenerator.generate(rand, maxDepth = maxDepth, allowActive = true))
  }

  private def singleExpressionBody(rand: Random) : Statement = {
    val ex = exprGenerator.generate(rand)
    Block(Seq(
      DefWire(NoInfo, "test", ex.tpe),
      Connect(NoInfo, Reference("test", ex.tpe), ex)
    ))
  }

  def generate(rand: Random) : Module = {
    val name = identifier(rand)
    // TODO: generate more general io ports!
    val reset = Port(NoInfo, "reset", Input, UIntType(IntWidth(1)))
    val clk = Port(NoInfo, "clk", Input, ClockType)
    val io = generateIO(rand)

    val body : Statement = singleExpressionBody(rand)

    Module(NoInfo, name, Seq(reset, clk, io), body)
  }
}
