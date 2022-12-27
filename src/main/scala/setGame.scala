package org.example

import com.sun.javafx.geom.ShapePair

object setGame extends App{

  case class game(shape: Shape, color: Color, number: Number, shading: Shading)

  enum Shape:
    case Oval, Squiggle, Diamond
  enum Color:
    case Red, Green, Purple
  enum Number:
    case One, Two, Three
  enum Shading:
    case Stripped, Open, Filled

  val deck = List(
    game(Shape.Oval, Color.Red, Number.One, Shading.Open),
    game(Shape.Diamond, Color.Green, Number.Two, Shading.Filled),
    game(Shape.Squiggle, Color.Purple, Number.Three, Shading.Stripped)
  )

  def isValid(card1: game, card2: game, card3: game): Boolean =
      checkShape(card1, card2, card3) &&
      checkColor(card1, card2, card3) &&
      checkNumber(card1, card2, card3) &&
      checkShading(card1, card2, card3)

  def checkShape(card1: game, card2: game, card3: game): Boolean =
    def allSame =
        card1.color == card2.color &&
        card1.color == card3.color
    def allDifferent =
           card1.shape != card2.shape &&
           card1.shape != card3.shape &&
           card2.shape != card3.shape
    allSame || allDifferent

  def checkColor(card1: game, card2: game, card3: game): Boolean =
    def allSame =
        card1.color == card2.color &&
        card1.color == card3.color
    def allDifferent =
      card1.color != card2.color &&
      card1.color != card2.color &&
      card1.color != card2.color
    allSame || allDifferent

  def checkNumber(card1: game, card2: game, card3: game): Boolean =
    def allSame =
        card1.number == card2.number &&
        card2.number == card3.number
    def allDifferent =
        card1.number != card2.number &&
        card1.number != card3.number &&
        card2.number != card3.number
    allSame || allDifferent

  def checkShading(card1: game, card2: game, card3: game): Boolean =
    def allSame =
        card1.shading == card2.shading &&
        card2.shading == card3.shading
    def allDifferent =
        card1.shading != card2.shading &&
        card1.shading != card3.shading &&
        card2.shading != card3.shading
    allSame || allDifferent

  val res = isValid(
    game(Shape.Oval, Color.Red, Number.Two, Shading.Filled),
    game(Shape.Diamond, Color.Purple, Number.Three, Shading.Stripped),
    game(Shape.Squiggle, Color.Green, Number.One, Shading.Open)
  )
  print(res)

  val res1 = isValid(
    game(Shape.Oval, Color.Red, Number.Three, Shading.Filled),
    game(Shape.Diamond, Color.Purple, Number.Two, Shading.Open),
    game(Shape.Squiggle, Color.Green, Number.One, Shading.Stripped)
  )
  print(res1)
}
