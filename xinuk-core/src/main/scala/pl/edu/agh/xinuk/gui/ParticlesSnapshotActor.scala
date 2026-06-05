package pl.edu.agh.xinuk.gui

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId
import pl.edu.agh.xinuk.simulation.WorkerActor.{GuiInfo, MsgWrapper, SubscribeGuiInfo}
import pl.edu.agh.xinuk.simulation.GuiCellColor

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable
import pl.edu.agh.xinuk.simulation.GuiCellParticles
import pl.edu.agh.xinuk.simulation.GuiParticle

class ParticlesSnapshotActor private (
    worker: ActorRef,
    simulationId: String,
    workerIds: Set[WorkerId]
)(implicit
    config: XinukConfig
) extends Actor
    with ActorLogging {

  private lazy val snapshotSaver: ParticlesSnapshotSaver = new ParticlesSnapshotSaver(simulationId)
  val cellParticlesStash: mutable.Map[Long, Seq[Seq[(CellId, GuiCellParticles)]]] =
    mutable.Map.empty.withDefaultValue(Seq.empty)

  override def receive: Receive = started

  override def preStart(): Unit = {
    workerIds.foreach(worker ! MsgWrapper(_, SubscribeGuiInfo()))
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
  }

  def started: Receive = { case GuiInfo(iteration, cellPayloads, metrics) =>
    val cellParticles = cellPayloads.map({
      case (cellId, cellPayload) => {
        val cellParticles = cellPayload.asInstanceOf[GuiCellParticles]
        (cellId, cellParticles)
      }
    })

    cellParticlesStash(iteration) :+= cellParticles.toSeq
    if (cellParticlesStash(iteration).size == workerIds.size) {
      snapshotSaver.snapshot(iteration, cellParticlesStash(iteration).flatten.toMap)
      cellParticlesStash.remove(iteration)
    }
  }
}

object ParticlesSnapshotActor {
  def props(worker: ActorRef, simulationId: String, workerIds: Set[WorkerId])(implicit
      config: XinukConfig
  ): Props = {
    Props(new ParticlesSnapshotActor(worker, simulationId, workerIds))
  }
}

private class ParticlesSnapshotSaver(simulationId: String)(implicit config: XinukConfig) {
  private val snapshotDirectory = new File(s"out/snapshots/$simulationId")
  snapshotDirectory.mkdirs()
  private val img = new BufferedImage(
    config.worldWidth * config.guiCellSize,
    config.worldHeight * config.guiCellSize,
    BufferedImage.TYPE_INT_ARGB
  )

  private def fillImage(cellParticles: Map[CellId, GuiCellParticles]): Unit = {
    val g = img.createGraphics()
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, img.getWidth(), img.getHeight())
    g.dispose()

    val particleSize = 1
    cellParticles.foreach {
      case (GridCellId(gridX, gridY), GuiCellParticles(particles, particlesColor)) =>
        val particleArray = Array.fill(particleSize * particleSize)(particlesColor.getRGB)
        val startX = gridX * config.guiCellSize
        val startY = gridY * config.guiCellSize
        particles.foreach { case GuiParticle(particleX, particleY) =>
          val pixelX = startX + (particleX * config.guiCellSize).toInt - particleSize / 2
          val pixelY = startY + (particleY * config.guiCellSize).toInt - particleSize / 2
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
  }

  def snapshot(iteration: Long, cellParticles: Map[CellId, GuiCellParticles]): Unit = {
    val snapshotFile = new File(snapshotDirectory, f"$iteration%09d.png")
    fillImage(cellParticles)
    ImageIO.write(img, "png", snapshotFile)
  }
}
