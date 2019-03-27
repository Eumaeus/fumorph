package edu.furman.classics.fumorph

import org.scalatest._
import edu.holycross.shot.cite._

class ExportTest extends FlatSpec {

   "The Morph JSON library" should "compile" in {
    val groupLevel = CtsUrn("urn:cts:greekLit:tlg0012:")
    assert(groupLevel.textGroup == "tlg0012")
  }

"The UrnGenerator" should "generate a urn." in {
      val urnBase:String = "urn:cite2:test:form.v1:"
      val newUrn:Cite2Urn = UrnGenerator.get("fumorph","test","temp")
      println(newUrn)
      assert( Cite2Urn(newUrn.toString) == newUrn )
  }
}
