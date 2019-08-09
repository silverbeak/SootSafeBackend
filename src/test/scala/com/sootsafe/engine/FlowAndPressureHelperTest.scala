package com.sootsafe.engine

import com.sootsafe.model._
import com.sootsafe.valuetable.SuppliedValueResolver
import org.scalatest.{Matchers, WordSpecLike}

class FlowAndPressureHelperTest extends WordSpecLike with Matchers {

  "GenerateRegularPressureLossTable" must {

    "generate table for basic data" in {
      val nodeDataArray = fakeNodeGenerator(6).toList
      val linkDataArray = createFakeLinks(nodeDataArray).toList

      val model = Model(nodeDataArray, linkDataArray)

      new ModelBuilder(model).buildModel() match {
        case Right(errorMessage) =>
          fail(errorMessage)

        case Left(linkedNodeModel) =>

          val table = FlowAndPressureHelper.generateRegularPressureLossTable(linkedNodeModel.locateTargetNode().get, SuppliedValueResolver)

          for {i <- 0 to 5} {
            table(i) should be(i)
          }

          table.size should be(6)
      }
    }
  }

  "GenerateAggregatedRegularPressureTable" must {
    "generate table for basic data" in {
      val nodeDataArray = fakeNodeGenerator(6).toList
      val linkDataArray = createFakeLinks(nodeDataArray).toList

      val model = Model(nodeDataArray, linkDataArray)

      new ModelBuilder(model).buildModel() match {
        case Right(errorMessage) =>
          fail(errorMessage)

        case Left(linkedNodeModel) =>

          val table = FlowAndPressureHelper.generateAggregatedRegularPressureLossTable(linkedNodeModel.locateTargetNode().get)

          table.size should be(6)

          table(0) should be(15)
          table(1) should be(15)
          table(2) should be(14)
          table(3) should be(12)
          table(4) should be(9)
          table(5) should be(5)

      }
    }
  }

  "GenerateRegularFlowTable" must {
    "generate table for basic data" in {
      val nodeDataArray = fakeNodeGenerator(6).toList
      val linkDataArray = createFakeLinks(nodeDataArray).toList

      val model = Model(nodeDataArray, linkDataArray)

      new ModelBuilder(model).buildModel() match {
        case Right(errorMessage) =>
          fail(errorMessage)

        case Left(linkedNodeModel) =>

          val table = FlowAndPressureHelper.generateRegularFlowTable(linkedNodeModel.locateTargetNode().get)

          table.size should be(6)

          table(0) should be(5)
          table(1) should be(2)
          table(2) should be(3)
          table(3) should be(2)
          table(4) should be(1)
          table(5) should be(1)

      }
    }
  }


  /**
    * Generates a sequence of fake node modules.
    * The first module will always be an outlet.
    * The second cell will always be a box.
    * The last module will always be a fireCell.
    * Any cell(s) in between (the box and the fire cell) will be alternating pipes and t-pipes
    * The SootSafeInfo objects will contain fake data as well. The capacity will equal the index of the module (ie, the third element will have a capacity of 3, the fourth will have 4 and so on). The same principle applies to dimensions and pressureLoss entries.
    * A shortcoming of this way of generating elements is that all indices will be in order, that is not the case when it comes to data that we receive from the frontend. Beware, and perhaps create specific test case for that.
    * The final module (the fireCell) will have the targetCell field set to true
    *
    * @param length The length of the sequence
    * @return A sequence of generated node modules, with corresponding SootSafeInfo entries.
    */
  private def fakeNodeGenerator(length: Int): Seq[NodeModule] = {
    def ssInfo(i: Int, nodeType: String): SootSafeInfo = {
      SootSafeInfo(
        nodeType = nodeType,
        capacity = Some(length - i),
        name = Some(s"Fake$nodeType - $i"),
        comment = Some(s"${nodeType}Comment - $i"),
        pressureloss = Some(i),
        dimension = Dimension(length = Some(i), diameter = Some(i)),
        targetCell = i == length - 1
      )
    }

    for {
      i <- 0 until length
    } yield {
      i match {
        case 0 => Outlet(i, ssInfo(i, "outlet"))
        case 1 => Box(i, ssInfo(i, "box"))
        case _ if i == length - 1 => FireCell(i, ssInfo(i, "fireCell"))
        case _ if i % 2 == 0 => Pipe(i, ssInfo(i, "pipe"))
        case _ => TPipe(i, ssInfo(i, "tpipe"))
      }
    }
  }

  /**
    * Takes a list of node modules and pairs them up, generating a sequence of links. The resulting sequence should always have length that is one element less than that of the supplied list of modules.
    *
    * @param nodes The list of nodes that will be used as a basis for creating the sequence of links
    * @return The generated sequence.
    */
  private def createFakeLinks(nodes: Seq[NodeModule]): Seq[Link] = {
    nodes
      .sliding(2)
      .map(nodePair => Link(
        from = nodePair.head.key,
        to = nodePair(1).key,
        fid = nodePair.head.key.toString,
        tid = nodePair(1).key.toString
      ))
  }.toSeq

}
