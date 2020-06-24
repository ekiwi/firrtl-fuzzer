package generators

// Copyright 2019 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

import firrtl.ir._

case class FirrtlTypeSettings(MaxLiteralBits: Int = 8, MinLiteralBits: Int = 8, MaxVectorLength: Int = 99, MaxBundleEntries: Int = 7, MaxIntBits: Int = 33)

case class FirrtlTypeProperties(maxDepth: Int = 3, allowActive: Boolean = true, allowVectors: Boolean = true, allowBundles: Boolean = true)

// WARN: does not generate Clock, Analog or Fixed type
class FirrtlTypeGenerator(settings: FirrtlTypeSettings = FirrtlTypeSettings()) extends FirrtlGenerator {

  def generateOrientation(rand: Random, prop: FirrtlTypeProperties) : Orientation = {
    if(prop.allowActive && rand.nextBoolean()) { Flip } else { Default }
  }

  def generateGround(rand: Random, minBits: Int = 0, maxBits: Int = settings.MaxIntBits) : Type = {
    rand.choose(Seq(generateSInt _, generateUInt _))(rand, minBits, maxBits)
  }

  def generateUInt(rand: Random, minBits: Int = 0, maxBits: Int = settings.MaxIntBits) : Type = {
    UIntType(IntWidth(rand.nextInt(minBits, maxBits)))
  }

  def generateSInt(rand: Random, minBits: Int = 0, maxBits: Int = settings.MaxIntBits) : Type = {
    SIntType(IntWidth(rand.nextInt(minBits, maxBits)))
  }

  def generateVector(rand: Random, prop: FirrtlTypeProperties, depth: Int) : Type = {
    val len = rand.nextInt(0, settings.MaxVectorLength)
    val tpe = generateType(rand, prop, depth + 1)
    VectorType(tpe=tpe, size=len)
  }

  def generateBundle(rand: Random, prop: FirrtlTypeProperties, depth: Int) : Type = {
    // bundles can be empty!
    val entryCount = rand.nextInt(0, settings.MaxBundleEntries)
    val entries : Seq[Field] = (0 until entryCount).map(_ => Field(
      identifier(rand, 7), generateOrientation(rand, prop), generateType(rand, prop, depth + 1)
    ))
    BundleType(entries)
  }

  def generateType(rand: Random, prop: FirrtlTypeProperties, depth: Int = 0) : Type = {
    val requireGround = depth + 1 >= prop.maxDepth
    def vec = if(!requireGround && prop.allowVectors) { Some( () => generateVector(rand, prop, depth)) } else { None }
    def bundle = if(!requireGround && prop.allowBundles) { Some(() => generateBundle(rand, prop, depth)) } else { None }
    val generators = Seq(Some(() => generateGround(rand)), vec, bundle).flatten
    rand.choose(generators)()
  }

  def generate(rand: Random, maxDepth: Int = 3, allowActive: Boolean = true, allowVectors: Boolean = true, allowBundles: Boolean = true) : Type = {
    generateType(rand, FirrtlTypeProperties(maxDepth, allowActive, allowVectors, allowBundles))
  }
}
