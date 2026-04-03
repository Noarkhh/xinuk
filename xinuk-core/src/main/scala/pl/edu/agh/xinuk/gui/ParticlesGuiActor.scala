package pl.edu.agh.xinuk.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import javax.swing.{ImageIcon, UIManager}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.xinuk.algorithm.Metrics
import pl.edu.agh.xinuk.config.{XinukConfig, CellGuiPayloadColor}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldShard}
import pl.edu.agh.xinuk.simulation.WorkerActor.{GuiInfo, MsgWrapper, SubscribeGuiInfo}
import pl.edu.agh.xinuk.simulation.GuiCellParticles

import scala.collection.mutable
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.util.{Random, Try}
import pl.edu.agh.xinuk.simulation.GuiParticle

class ParticlesGuiActor private (
    worker: ActorRef,
    simulationId: String,
    workerId: WorkerId,
    bounds: GridWorldShard.Bounds
)(implicit config: XinukConfig)
    extends Actor
    with ActorLogging {

  override def receive: Receive = started

  private lazy val gui: GuiParticles = new GuiParticles(bounds, workerId)

  override def preStart(): Unit = {
    worker ! MsgWrapper(workerId, SubscribeGuiInfo())
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
    gui.quit()
  }

  def started: Receive = { case GuiInfo(iteration, cellPayloads, metrics) =>
    val cellParticlesMap = cellPayloads.map({
      case (cellId, cellPayload) => {
        val cellParticles = cellPayload.asInstanceOf[GuiCellParticles]
        (cellId, cellParticles)
      }
    })
    gui.setNewValues(cellParticlesMap)
    gui.updatePlot(iteration, metrics)
  }
}

object ParticlesGuiActor {
  def props(
      worker: ActorRef,
      simulationId: String,
      workerId: WorkerId,
      bounds: GridWorldShard.Bounds
  )(implicit config: XinukConfig): Props = {
    Props(new ParticlesGuiActor(worker, simulationId, workerId, bounds))
  }
}

private[gui] class GuiParticles(bounds: GridWorldShard.Bounds, workerId: WorkerId)(implicit
    config: XinukConfig
) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgColor = new Color(220, 220, 220)
  private val cellView =
    new ParticleCanvas(bounds.xMin, bounds.yMin, bounds.xSize, bounds.ySize, config.guiCellSize)
  private val chartPanel = new BorderPanel {
    background = bgColor
  }
  private val chartPage = new Page("Plot", chartPanel)
  private val (alignedLocation, alignedSize) = alignFrame()

  def top: MainFrame = new MainFrame {
    title = s"Xinuk ${workerId.value}"
    background = bgColor
    location = alignedLocation
    preferredSize = alignedSize

    val mainPanel: BorderPanel = new BorderPanel {

      val cellPanel: BorderPanel = new BorderPanel {
        val view: BorderPanel = new BorderPanel {
          background = bgColor
          layout(cellView) = Center
        }
        background = bgColor
        layout(view) = Center
      }

      val contentPane: TabbedPane = new TabbedPane {
        pages += new Page("Cells", cellPanel)
        pages += chartPage
      }

      layout(contentPane) = Center
    }

    contents = mainPanel
  }

  private def alignFrame(): (Point, Dimension) = {
    val xPos = (workerId.value - 1) / config.workersY
    val yPos = (workerId.value - 1) % config.workersY

    val xGlobalOffset = 100
    val yGlobalOffset = 0

    val xWindowAdjustment = 24
    val yWindowAdjustment = 70

    val xLocalOffset = bounds.xMin * config.guiCellSize + xPos * xWindowAdjustment
    val yLocalOffset = bounds.yMin * config.guiCellSize + yPos * yWindowAdjustment

    val width = bounds.xSize * config.guiCellSize + xWindowAdjustment
    val height = bounds.ySize * config.guiCellSize + yWindowAdjustment

    val location = new Point(xGlobalOffset + xLocalOffset, yGlobalOffset + yLocalOffset)
    val size = new Dimension(width, height)
    (location, size)
  }

  def setNewValues(cellParticlesMap: Map[CellId, GuiCellParticles]): Unit = {
    cellView.set(cellParticlesMap)
  }

  private class ParticleCanvas(xOffset: Int, yOffset: Int, xSize: Int, ySize: Int, guiCellSize: Int)
      extends Label {
    private val img =
      new BufferedImage(xSize * guiCellSize, ySize * guiCellSize, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cellParticlesMap: Map[CellId, GuiCellParticles]): Unit = {
      val g = img.createGraphics()
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, img.getWidth, img.getHeight)
      g.setColor(new Color(180, 180, 180))
      for (col <- 1 until xSize) {
        g.drawLine(col * guiCellSize, 0, col * guiCellSize, img.getHeight)
      }
      for (row <- 1 until ySize) {
        g.drawLine(0, row * guiCellSize, img.getWidth, row * guiCellSize)
      }
      g.dispose()

      val particleSize = 1

      cellParticlesMap.foreach {
        case (GridCellId(gridX, gridY), GuiCellParticles(particles, particlesColor)) =>
          val particleArray = Array.fill(particleSize * particleSize)(particlesColor.getRGB)
          val startX = (gridX - xOffset) * guiCellSize
          val startY = (gridY - yOffset) * guiCellSize
          particles.foreach { case GuiParticle(particleX, particleY) =>
            val pixelX = startX + (particleX * guiCellSize).toInt - particleSize / 2
            val pixelY = startY + (particleY * guiCellSize).toInt - particleSize / 2
            val clampedX = pixelX.max(0).min(img.getWidth - particleSize)
            val clampedY = pixelY.max(0).min(img.getHeight - particleSize)
            img.setRGB(
              clampedX,
              clampedY,
              particleSize,
              particleSize,
              particleArray,
              0,
              particleSize
            )
          }
        case _ =>
      }
      this.repaint()
    }
  }

  private val nameToSeries = mutable.Map.empty[String, XYSeries]
  private val dataset = new XYSeriesCollection()
  private val chart = ChartFactory.createXYLineChart(
    "Iteration metrics chart",
    "X",
    "Y",
    dataset,
    PlotOrientation.VERTICAL,
    true,
    true,
    false
  )
  private val panel = new ChartPanel(chart)
  chartPanel.layout(swing.Component.wrap(panel)) = Center

  def updatePlot(iteration: Long, metrics: Metrics): Unit = {
    def createSeries(name: String): XYSeries = {
      val series = new XYSeries(name)
      series.setMaximumItemCount(GuiParticles.MaximumPlotSize)
      dataset.addSeries(series)
      series
    }

    metrics.series.foreach { case (name, value) =>
      nameToSeries.getOrElseUpdate(name, createSeries(name)).add(iteration.toDouble, value)
    }
  }

  main(Array.empty)

}

object GuiParticles {
  final val MaximumPlotSize = 400
}
