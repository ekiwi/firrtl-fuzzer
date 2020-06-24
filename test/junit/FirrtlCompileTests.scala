package junit

import com.pholser.junit.quickcheck._
import com.pholser.junit.quickcheck.generator._
import com.pholser.junit.quickcheck.random.SourceOfRandomness
import com.pholser.junit.quickcheck.runner.JUnitQuickcheck
import firrtl.{ChirrtlForm, CircuitState, MiddleFirrtlCompiler, Parser, Transform}
import org.junit.Assert._
import org.junit.runner.RunWith
import firrtl.ir._

class FirrtlSingleModuleGenerator extends Generator[Circuit](classOf[Circuit]) {
  override def generate(random: SourceOfRandomness, status: GenerationStatus) : Circuit = {
    val settings = generators.FirrtlModuleSettings()
    val gen = new generators.FirrtlModuleGenerator(settings)
    val m = gen.generate(new generators.JUnitRandom(random))
    Circuit(NoInfo, Seq(m), m.name)
  }
}

@RunWith(classOf[JUnitQuickcheck])
class FirrtlCompileTests {
  @Property
  def compileSingleModule(@From(value = classOf[FirrtlSingleModuleGenerator]) c: Circuit) = {
    compile(c)
  }


  // adapted from chisel3.Driver.execute and firrtl.Driver.execute
  def compile(c: Circuit) = {
    //val compiler = new LowFirrtlCompiler()
    val compiler = new MiddleFirrtlCompiler()
    try {
      val res = compiler.compile(CircuitState(c, ChirrtlForm, Seq()), Seq())
    } catch {
      case any : Throwable => assert(false, c.serialize + "\n" + any.toString)
    }

  }
}