import scala.xml._

/**
 * A converter from leo outliner to FreeMind.
 * Clones in Leo are converted to local links in FreeMind.
 *    
 * Usage:
 *   compile: download scala 2.8, put its bin in the path and then: scalac leo2mm.scala 
 *   run: scala leo2mm input.leo output.mm
 *   open the mm file in notepad++ and choose encoding / convert to UTF-8.
 *   -> the mm file can be opened in FreeMind 
 */ 
class leo2mm(all: NodeSeq) {

  // map ID -> label
  val vnodes = Map.empty ++ { for (v <- all \ "vnodes" \\ "v" if (!(v \ "vh").isEmpty))
                              yield (v \ "@t" text, v \ "vh" text) }
  
  // map ID -> contents
  val tnodes = Map.empty ++ { for (t <- all \ "tnodes" \ "t" if (!t.text.isEmpty))
                              yield (t \ "@tx" text, t text) }
  
  def processNodes(vs: NodeSeq): NodeSeq = {
    for (v <- vs) yield {
      val id = v \ "@t" text;
      if ((v \ "vh").isEmpty)
        <node ID={ id+"link" } TEXT={ vnodes(id) } LINK={ "#"+id }/>
      else
        <node ID={ id } TEXT={ vnodes(id) }>{
          if (tnodes contains id)
            <richcontent TYPE="NOTE"><pre>{ tnodes(id) }</pre></richcontent>
        } { processNodes(v \ "v") }
        </node>
    }
  }
}

object leo2mm {
  def main(args: Array[String]) {
    val all = XML.loadFile(args(0))
    val map = <map version="0.9.0"><node ID="root" TEXT="imported">{ 
                 new leo2mm(all).processNodes(all \ "vnodes" \ "v") }</node></map>
    XML.save(args(1), map, "UTF-8", false, null)
    //TODO FreeMind can only open the file if it has a BOM, how to tell XML.save to put one?
  }
}

//leo2mm.main(Array("perso.leo", "out.mm"))
