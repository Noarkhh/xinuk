package pl.edu.agh.rabbits

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.rabbits.algorithm.{
  RabbitsMetrics,
  RabbitsPlanCreator,
  RabbitsPlanResolver,
  RabbitsWorldCreator
}
import pl.edu.agh.rabbits.model.{Lettuce, Rabbit}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.CellState
import pl.edu.agh.xinuk.model.grid.GridSignalPropagation
import pl.edu.agh.xinuk.simulation.GridInfoCellColor
import pl.edu.agh.xinuk.algorithm.Metrics

object RabbitsMain extends LazyLogging {
  private val configPrefix = "rabbits"

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      RabbitsMetrics.MetricHeaders,
      RabbitsWorldCreator,
      RabbitsPlanCreator,
      RabbitsPlanResolver,
      RabbitsMetrics.empty,
      GridSignalPropagation.Standard,
      cellStatePayloader
    ).start()
  }

  private def cellStatePayloader(cellState: CellState): GridInfoCellColor = {
    GridInfoCellColor(cellState.contents match {
      case _: Rabbit  => new Color(140, 69, 19)
      case _: Lettuce => new Color(0, 128, 0)
      case _          => Color.WHITE
    })
  }

  // private def cellToPayload(cellState: CellState): CellGuiPayload = {
  //   val color =
  //     cellState.contents match {
  //       case _: Rabbit  => new Color(140, 69, 19)
  //       case _: Lettuce => new Color(0, 128, 0)
  //       case _          => Color.WHITE
  //     }
  //
  //   CellGuiPayloadColor(color)
  // }
}
