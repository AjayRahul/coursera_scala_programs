package org.example

import Driver.Network

object energyImpactOnEnv extends App{

  case class Video(Duration: Int, Bytes: Double, Network: Network)

  val Duration = 30 * 60 // in seconds
  val lowQualityByte = 0.3 // MB/s
  val highQualityByte = 0.6 // MB/s
  val co2PerKwh = 0.5
  val dataCentreEnergy = 0.00072

  enum Network:
    case Fixed, Mobile

  val lowQuality = Video(Duration, lowQualityByte, Network.Fixed)
  val highQuality = Video(Duration, highQualityByte, Network.Mobile)

  def networkEnergy(network: Network): Double = network match
    case Network.Fixed => 0.00043
    case Network.Mobile => 0.00088

  def footPrint(video: Video): Double =
    val megaByte = video.Duration * video.Bytes
    val energy = dataCentreEnergy + networkEnergy(video.Network)
    energy * megaByte * co2PerKwh

  println(footPrint(lowQuality)) // output: 0.3105
  println(footPrint(highQuality))  // output: 0.864

}
