package de.tubs.cs.ias.plotalyzer.utility.color

import java.awt.Color
import java.awt.image.BufferedImage

object ColorRepresentation {

  case class RGB(r: Int, g: Int, b: Int)

  case class Lab(L: Double, a: Double, b: Double)

  // credits to: https://stackoverflow.com/a/45263428/919434
  def rgbToLab(color: RGB): Lab = {
    val Xr: Double = 95.047
    val Yr: Double = 100.0
    val Zr: Double = 108.883
    // --------- RGB to XYZ ---------//
    var r: Double = color.r / 255.0
    var g: Double = color.g / 255.0
    var b: Double = color.b / 255.0
    if (r > 0.04045) r = Math.pow((r + 0.055) / 1.055, 2.4)
    else r = r / 12.92
    if (g > 0.04045) g = Math.pow((g + 0.055) / 1.055, 2.4)
    else g = g / 12.92
    if (b > 0.04045) b = Math.pow((b + 0.055) / 1.055, 2.4)
    else b = b / 12.92
    r *= 100
    g *= 100
    b *= 100
    val X: Double = 0.4124 * r + 0.3576 * g + 0.1805 * b
    val Y: Double = 0.2126 * r + 0.7152 * g + 0.0722 * b
    val Z: Double = 0.0193 * r + 0.1192 * g + 0.9505 * b
    // --------- XYZ to Lab --------- //
    var xr: Double = X / Xr
    var yr: Double = Y / Yr
    var zr: Double = Z / Zr

    if (xr > 0.008856) xr = Math.pow(xr, 1 / 3d)
    else xr = ((7.787 * xr) + 16.0 / 116.0)

    if (yr > 0.008856) yr = Math.pow(yr, 1 / 3d)
    else yr = ((7.787 * yr) + 16.0 / 116.0)

    if (zr > 0.008856) {
      zr = Math.pow(zr, 1 / 3d)
    } else {
      zr = ((7.787 * zr) + 16.0 / 116.0)
    }

    Lab((116 * yr) - 16, 500 * (xr - yr), 200 * (yr - zr))
  }

  def deltaE(lhs: RGB, rhs: RGB): Double = {
    deltaE(rgbToLab(lhs), rgbToLab(rhs))
  }

  private def radians(deg: Double): Double = {
    deg * Math.PI / 180.0
  }

  private def degree(rad: Double): Double = {
    (rad * 180.0) / Math.PI

  }

  def atan2(b: Double, a: Double): Double = {
    degree(Math.atan2(radians(b), radians(a)))
  }

  def cos(b: Double): Double = {
    degree(Math.cos(radians(b)))
  }

  def sin(b: Double): Double = {
    degree(Math.sin(radians(b)))
  }

  def deltaE(lhs: Lab, rhs: Lab): Double = {
    CIDE2000.calculateDeltaE(lhs, rhs)
  }

  def colorOfImage(image: BufferedImage): RGB = {
    var rsum = 0
    var gsum = 0
    var bsum = 0
    var counter = 0
    (0 until image.getWidth).foreach { width =>
      (0 until image.getHeight).foreach { height =>
        val pixel = new Color(image.getRGB(width, height))
        rsum = rsum + pixel.getRed
        gsum = gsum + pixel.getGreen
        bsum = bsum + pixel.getBlue
        counter = counter + 1
      }
    }
    RGB(rsum / counter, gsum / counter, bsum / counter)
  }

}
