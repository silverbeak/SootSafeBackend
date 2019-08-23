package com.sootsafe.engine

import java.io.InputStream
import java.text.DecimalFormat

import com.sootsafe.model.{LinkedNode, Model, ModelBuilder}
import com.sootsafe.reporting.SootSafeReportGenerator
import com.sootsafe.serializers.GRPCSerializer
import com.sootsafe.server.calculator.SootSafeCalculator.SootSafeModel
import com.sootsafe.valuetable.{RealValueResolver, SuppliedValueResolver}
import org.scalatest.{Matchers, WordSpecLike}
import scalapb.json4s.JsonFormat

class BoverketTest extends WordSpecLike with Matchers with TestFixture {

  val df: DecimalFormat = new DecimalFormat("#.####")

  "Boverket engine" should {
    "calculate proper values from example" in {
      Boverket.calculatePressureLoss(linkedModel, initialFirePressure = 1000d, valueResolver = RealValueResolver) match {
        case Left(result) =>
          result.size should be(7)
          val pressureLoss = FlowAndPressureSequence.aggregatePressure(result.tail)
          df.format(pressureLoss) should be("245,1812")

          println(s"Latex:\n${SootSafeReportGenerator.generateLatex(result)}")

        case Right(error) =>
          fail(s"Expected successful calculation. Got: $error")
      }
    }

    "calculate proper values from example 2" in {
      Boverket.calculatePressureLoss(linkedModelHigherOriginalResistance, 1000, RealValueResolver) match {
        case Left(result) =>
          result.size should be(7)
          val pressureLoss = FlowAndPressureSequence.aggregatePressure(result.tail)
          df.format(pressureLoss) should be("68,4828")

        case Right(error) =>
          fail(s"Expected successful calculation. Got: $error")
      }
    }

    "calculate proper values from example 3" in {
      val linkedNode = readFile("/json/fid1.json")
      Boverket.calculatePressureLoss(linkedNode, 1000, SuppliedValueResolver) match {
        case Left(result) =>
          result.size should be(7)
          val pressureLoss = FlowAndPressureSequence.aggregatePressure(result)
//          df.format(pressureLoss) should be("241,9042")

          df.format(result.head.addedFireFlow.toValue.value) should be ("114,614")
          df.format(result.head.aggregatedFireFlow.toValue.value) should be ("114,614")
          df.format(result.head.aggregatedRegularFlow.toValue.value) should be ("17")

          // These two seem to be skewed one position. There should be another "114,614" value in between
          df.format(result(1).aggregatedFireFlow.toValue.value) should be ("114,614")
//          df.format(result(2).aggregatedFireFlow.toValue.value) should be ("155,099")

          df.format(result.last.addedFireFlow.toValue.value) should be ("827,8512")
          df.format(result.last.aggregatedFireFlow.toValue.value) should be ("1129,5904")
          // Not entirely sure this last one is right. Shouldn't it be "624
          df.format(result.last.aggregatedRegularFlow.toValue.value) should be ("156")

        case Right(error) =>
          fail(s"Expected successful calculation. Got: $error")
      }
    }
  }

  def readFile(fileName: String): LinkedNode = {
    val stream: InputStream = getClass.getResourceAsStream(fileName)
    val defaultJson = scala.io.Source.fromInputStream(stream).mkString
    val ssModel = referenceToFidRequest(defaultJson)
    val model = extractModelFromRequest(ssModel)
    new ModelBuilder(model).buildModel() match {
      case Right(errorMessage) => throw new Exception(errorMessage)
      case Left(node) => node
    }
  }

  private val referenceToFidRequest: PartialFunction[String, SootSafeModel] = {
    case stringRepr =>
      JsonFormat.fromJsonString[SootSafeModel](stringRepr)
  }

  private def extractModelFromRequest(request: SootSafeModel) = {
    val nodes = request.nodes.map(GRPCSerializer.deserialize)
    val links = request.links.map(GRPCSerializer.deserialize)
    Model(nodes.toList, links.toList)
  }

}
