package pl.edu.agh.xinuk.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import javax.swing.{ImageIcon, UIManager}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.xinuk.algorithm.Metrics
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId
import pl.edu.agh.xinuk.simulation.WorkerActor.{GuiInfo, MsgWrapper, SubscribeGuiInfo}
import pl.edu.agh.xinuk.simulation.{GuiCellParticles, GuiParticle}

import scala.collection.mutable
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.util.Try

class MicroParticlesGuiActor private (
    worker: ActorRef,
    simulationId: String,
    workerIds: Set[WorkerId]
)(implicit config: XinukConfig)
    extends Actor
    with ActorLogging {

  private lazy val gui: GuiMicroParticles = new GuiMicroParticles()

  private val cellParticlesStash: mutable.Map[Long, Seq[Seq[(CellId, GuiCellParticles)]]] =
    mutable.Map.empty.withDefaultValue(Seq.empty)
  private val metricsStash: mutable.Map[Long, Seq[Metrics]] =
    mutable.Map.empty.withDefaultValue(Seq.empty)

  override def receive: Receive = started

  override def preStart(): Unit = {
    workerIds.foreach(worker ! MsgWrapper(_, SubscribeGuiInfo()))
    log.info("Joint GUI started")
  }

  override def postStop(): Unit = {
    log.info("Joint GUI stopped")
    gui.quit()
  }

  def started: Receive = { case GuiInfo(iteration, cellPayloads, metrics) =>
    val cellParticles = cellPayloads.map { case (cellId, cellPayload) =>
      (cellId, cellPayload.asInstanceOf[GuiCellParticles])
    }

    cellParticlesStash(iteration) :+= cellParticles.toSeq
    metricsStash(iteration) :+= metrics
    // metricsStash(iteration) = metricsStash.get(iteration).fold(metrics)(_ + metrics)

    if (cellParticlesStash(iteration).size == workerIds.size) {
      gui.setNewValues(cellParticlesStash(iteration).flatten.toMap)
      gui.updatePlot(iteration, metricsStash(iteration).reduce(_ + _))
      cellParticlesStash.remove(iteration)
      metricsStash.remove(iteration)
    }
  }
}

object MicroParticlesGuiActor {
  def props(worker: ActorRef, simulationId: String, workerIds: Set[WorkerId])(implicit
      config: XinukConfig
  ): Props =
    Props(new MicroParticlesGuiActor(worker, simulationId, workerIds))
}

private[gui] class GuiMicroParticles()(implicit config: XinukConfig)
    extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgColor = new Color(220, 220, 220)
  private val cellView =
    new ParticleCanvas(
      config.worldWidth,
      config.worldHeight,
      config.guiCellSize,
      config.guiParticleSize
    )
  private val chartPanel = new BorderPanel {
    background = bgColor
  }

  def top: MainFrame = new MainFrame {
    title = "Xinuk Joint View"
    background = bgColor
    preferredSize = new Dimension(
      config.worldWidth * config.guiCellSize + 24,
      config.worldHeight * config.guiCellSize + 70
    )

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
        pages += new Page("Plot", chartPanel)
      }

      layout(contentPane) = Center
    }

    contents = mainPanel
  }

  def setNewValues(cellParticlesMap: Map[CellId, GuiCellParticles]): Unit =
    cellView.set(cellParticlesMap)

  private class ParticleCanvas(xSize: Int, ySize: Int, guiCellSize: Int, particleSize: Int)
      extends Label {
    private val img =
      new BufferedImage(xSize * guiCellSize, ySize * guiCellSize, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cellParticlesMap: Map[CellId, GuiCellParticles]): Unit = {
      val g = img.createGraphics()
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, img.getWidth, img.getHeight)
      // g.setColor(new Color(180, 180, 180))
      // for (col <- 1 until xSize) {
      //   g.drawLine(col * guiCellSize, 0, col * guiCellSize, img.getHeight)
      // }
      // for (row <- 1 until ySize) {
      //   g.drawLine(0, row * guiCellSize, img.getWidth, row * guiCellSize)
      // }
      g.dispose()

      cellParticlesMap.foreach {
        case (GridCellId(gridX, gridY), GuiCellParticles(particles)) =>
          val particleArray = Array.fill(particleSize * particleSize)(Color.BLACK.getRGB())
          val startX = gridX * guiCellSize
          val startY = gridY * guiCellSize
          particles
            .groupMapReduce(particle => {
              (
                startX + (particle.x * guiCellSize).toInt - particleSize / 2,
                startY + (particle.y * guiCellSize).toInt - particleSize / 2
              )
            })(_ => 1)(_ + _)
            .foreach({ case ((x, y), n) =>
              val clampedX = x.max(0).min(img.getWidth - particleSize)
              val clampedY = y.max(0).min(img.getHeight - particleSize)
              val color = new Color(255, (255 - n * 20).max(0), 0).getRGB()

              img.setRGB(clampedX, clampedY, color)
            // img.setRGB(
            //   clampedX,
            //   clampedY,
            //   particleSize,
            //   particleSize,
            //   particleArray,
            //   0,
            //   particleSize
            // )

            })
        // particles.foreach { case GuiParticle(particleX, particleY) =>
        //   val pixelX = startX + (particleX * guiCellSize).toInt - particleSize / 2
        //   val pixelY = startY + (particleY * guiCellSize).toInt - particleSize / 2
        //   val clampedX = pixelX.max(0).min(img.getWidth - particleSize)
        //   val clampedY = pixelY.max(0).min(img.getHeight - particleSize)
        //   img.setRGB(
        //     clampedX,
        //     clampedY,
        //     particleSize,
        //     particleSize,
        //     particleArray,
        //     0,
        //     particleSize
        //   )
        // }
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
  chartPanel.layout(swing.Component.wrap(new ChartPanel(chart))) = Center

  def updatePlot(iteration: Long, metrics: Metrics): Unit = {
    def createSeries(name: String): XYSeries = {
      val series = new XYSeries(name)
      series.setMaximumItemCount(GuiMicroParticles.MaximumPlotSize)
      dataset.addSeries(series)
      series
    }
    metrics.series.foreach { case (name, value) =>
      nameToSeries.getOrElseUpdate(name, createSeries(name)).add(iteration.toDouble, value)
    }
  }

  main(Array.empty)
}

object GuiMicroParticles {
  final val MaximumPlotSize = 400
}
