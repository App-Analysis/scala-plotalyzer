package de.halcony.plotalyzer.utility.color

import ColorRepresentation.Lab

object CIDE2000 {

  def calculateDeltaE(lhs: Lab, rhs: Lab): Double = {
    val L1 = lhs.L
    val a1 = lhs.a
    val b1 = lhs.b

    val L2 = rhs.L
    val a2 = rhs.a
    val b2 = rhs.b

    val Lmean = (L1 + L2) / 2.0 //ok
    val C1 = Math.sqrt(a1 * a1 + b1 * b1)
    val C2 = Math.sqrt(a2 * a2 + b2 * b2)
    val Cmean = (C1 + C2) / 2.0

    val G = (1 - Math.sqrt(
      Math.pow(Cmean, 7) / (Math.pow(Cmean, 7) + Math.pow(25, 7)))) / 2
    val a1prime = a1 * (1 + G)
    val a2prime = a2 * (1 + G)

    val C1prime = Math.sqrt(a1prime * a1prime + b1 * b1)
    val C2prime = Math.sqrt(a2prime * a2prime + b2 * b2)
    val Cmeanprime = (C1prime + C2prime) / 2 //ok

    val h1prime = Math.atan2(b1, a1prime) + 2 * Math.PI * (if (Math.atan2(
                                                                 b1,
                                                                 a1prime) < 0) 1
                                                           else 0)
    val h2prime = Math.atan2(b2, a2prime) + 2 * Math.PI * (if (Math.atan2(
                                                                 b2,
                                                                 a2prime) < 0) 1
                                                           else 0)
    val Hmeanprime =
      if ((Math.abs(h1prime - h2prime) > Math.PI))
        (h1prime + h2prime + 2 * Math.PI) / 2
      else (h1prime + h2prime) / 2

    val T = 1.0 - 0.17 * Math.cos(Hmeanprime - Math.PI / 6.0) + 0.24 * Math.cos(
      2 * Hmeanprime) + 0.32 * Math.cos(3 * Hmeanprime + Math.PI / 30) - 0.2 * Math
      .cos(4 * Hmeanprime - 21 * Math.PI / 60)

    val deltahprime =
      if ((Math.abs(h1prime - h2prime) <= Math.PI)) h2prime - h1prime
      else if ((h2prime <= h1prime)) h2prime - h1prime + 2 * Math.PI
      else h2prime - h1prime - 2 * Math.PI

    val deltaLprime = L2 - L1
    val deltaCprime = C2prime - C1prime
    val deltaHprime = 2.0 * Math.sqrt(C1prime * C2prime) * Math.sin(
      deltahprime / 2.0)
    val SL = 1.0 + ((0.015 * (Lmean - 50) * (Lmean - 50)) / Math.sqrt(
      20 + (Lmean - 50) * (Lmean - 50)))
    val SC = 1.0 + 0.045 * Cmeanprime
    val SH = 1.0 + 0.015 * Cmeanprime * T

    val deltaTheta = (30 * Math.PI / 180) * Math.exp(
      -((180 / Math.PI * Hmeanprime - 275) / 25) * ((180 / Math.PI * Hmeanprime - 275) / 25))
    val RC = 2 * Math.sqrt(
      Math.pow(Cmeanprime, 7) / (Math.pow(Cmeanprime, 7) + Math.pow(25, 7)))
    val RT = -(RC) * Math.sin(2 * deltaTheta)

    val KL = 1
    val KC = 1
    val KH = 1

    Math.sqrt(
      ((deltaLprime / (KL * SL)) * (deltaLprime / (KL * SL))) + ((deltaCprime / (KC * SC)) * (deltaCprime / (KC * SC))) + ((deltaHprime / (KH * SH)) * (deltaHprime / (KH * SH))) + (RT * (deltaCprime / (KC * SC)) * (deltaHprime / (KH * SH))))
  }

}
