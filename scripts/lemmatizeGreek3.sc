
import scala.io.Source
import java.io._
import java.util.Calendar
import scala.collection.mutable.LinkedHashMap
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import scala.io.StdIn.readLine
import scala.collection.mutable._
import edu.furman.classics.fumorph._
import java.util.Calendar

val cex:String = """urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.head.0#ΚΑΤΑ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.head.1#ΙΩΑΝΝΗΝ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.0#Ἐν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.1#ἀρχῇ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.2#ἦν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.3#ὁ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.4#λόγος,
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.5#καὶ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.6#ὁ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.7#λόγος
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.8#ἦν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.9#πρὸς
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.10#τὸν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.11#θεόν,
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.12#καὶ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.13#θεὸς
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.14#ἦν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.15#ὁ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.1.16#λόγος.
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.0#οὗτος
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.1#ἦν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.2#ἐν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.3#ἀρχῇ
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.4#πρὸς
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.5#τὸν
urn:cts:greekLit:tlg0031.tlg004.wh_fu.token:1.2.6#θεόν."""

val nodes:Vector[CitableNode] = {
	cex.lines.toVector.map(l => {
		val u = CtsUrn(l.split("#")(0))
		val t = l.split("#")(1)
		CitableNode(u,t)
	})
}

val corp:Corpus = Corpus(nodes)

def collapseNodes(c:Corpus, exemplar:String = "_merged"):Corpus = {
	// check for validity
	val isOkay:Boolean = {
		val uniformDepth:Boolean = {
			c.nodes.map( _.urn.citationDepth).distinct.size == 1
		}
		val deeperThanOne:Boolean = {
			c.nodes.head.urn.citationDepth.head > 1
		}
		uniformDepth & deeperThanOne
	}	
	if (isOkay == false) throw new Exception(s"Nodes are not at the same citation level.")
		
	/* Example of how to use groupBy in Scala without losing object order */
	val grouped:Vector[(CtsUrn, Vector[(CitableNode,Int)])] = {
		c.nodes.zipWithIndex.groupBy(_._1.urn.collapsePassageBy(1)).toVector
	}
	val sorted1:Vector[(CtsUrn, Vector[(CitableNode,Int)])] = {
		grouped.map( g => {
			val u = g._1
			val v = g._2.sortBy(_._2)
			(u, v)
		})
	}
	val sorted2:Vector[(CtsUrn, Vector[(CitableNode,Int)])] = {
		grouped.sortBy(_._2.head._2)
	}
	
	val newNodes:Vector[CitableNode] = {
		sorted2.map( g => {
			val exemp:String = g._1.exemplar + exemplar
			val u:CtsUrn = g._1.dropExemplar.addExemplar(exemp)
			val t:String = g._2.map(_._1.text).mkString(" ")
			CitableNode(u,t)
		})
	}
	Corpus(newNodes)
	
}