import scala.xml._
import scala.util.control.Exception.ultimately

import java.io.FileOutputStream
import java.nio.channels.Channels


/**
 * A converter from leo outliner to FreeMind.
 * Clones in Leo are converted to local links in FreeMind.
 *    
 * Usage:
 *   compile: download scala 2.8, put its bin in the path and then: scalac leo2mm.scala 
 *   run: scala leo2mm input.leo output.mm
 * 
 * Known bugs:
 *   UTF-8 characters in text are not converted correctly
 *   a warning when opening FreeMind that the file is in an old version
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
	val  inputFileName = args(0)
	val outputFileName = args(1)
    val all = XML.loadFile(inputFileName)
    val map = <map version="0.9.0"><node ID="root" TEXT="imported">{ 
                 new leo2mm(all).processNodes(all \ "vnodes" \ "v") }</node></map>
    save(outputFileName, map)
  }
  
  /**
   * Saves the given xml to a file in UTF-8 encoding, with BOM.
   */
  def save(fileName: String, xml: Node) {
	val enc = "UTF-8"
	val fos = new FileOutputStream(fileName)
	val bom = Array(0xEF, 0xBB, 0xBF) map (_ toByte)
	fos.write(bom)
    val w = Channels.newWriter(fos.getChannel(), enc)
    ultimately(w.close())(
      XML.write(w, xml, enc, false, null)
    )
  }
}
