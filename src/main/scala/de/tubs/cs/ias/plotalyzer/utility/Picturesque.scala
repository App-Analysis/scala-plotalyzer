package de.tubs.cs.ias.plotalyzer.utility

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

object Picturesque {

  def getBufferedImage(bytes: Array[Byte]): BufferedImage = {
    val inputStream = new ByteArrayInputStream(bytes)
    ImageIO.read(inputStream)
  }

}
