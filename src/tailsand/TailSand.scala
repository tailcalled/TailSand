package tailsand

import javax.swing.JPanel
import javax.swing.JFrame
import java.awt.BorderLayout
import javax.swing.JList
import javax.swing.BoxLayout
import java.awt.GridLayout
import javax.swing.BorderFactory
import javax.swing.JTextField
import javax.swing.GroupLayout
import javax.swing.JLabel
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import javax.swing.JTextArea
import javax.swing.JScrollPane
import javax.swing.SpinnerNumberModel
import javax.swing.JSpinner
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import javax.swing.DefaultListCellRenderer
import javax.swing.ListCellRenderer
import tailsand.mods.Mod
import java.io.File
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import javax.swing.JDialog
import java.awt.Color
import javax.swing.JButton
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.WindowConstants

object TailSand {
  
  def main(args: Array[String]) = {
    val mods = new File("TailSand/").listFiles().filter(_.getName.endsWith(".ts")).map { modFile =>
      Try(Mod.load(modFile)) match {
        case Success(mod) => Right(mod)
        case Failure(_) => Left(modFile.getName)
      }
    }.toVector
    val modSelector = new ModSelector(mods)
    modSelector.showDialog()
  }
  
}
class ModSelector(mods: Vector[Either[String, SandMod]]) {
  
  private val modsDialog = new JDialog(null: JFrame, "Pick a mod")
  private val panel = new JPanel()
  
  panel.setLayout(new BorderLayout(5, 5))
  
  val modList = new JList(mods.toArray)
  modList.setBorder(BorderFactory.createEtchedBorder())
  modList.setCellRenderer(new ListCellRenderer[Either[String, SandMod]]() {
    val renderer = new DefaultListCellRenderer()
    def getListCellRendererComponent(
        list: JList[_ <: Either[String, SandMod]], value: Either[String, SandMod],
        index: Int, isSelected: Boolean, cellHasFocus: Boolean) = {
      renderer.asInstanceOf[ListCellRenderer[Object]].getListCellRendererComponent(list, value, index, isSelected, cellHasFocus)
      value match {
        case Left(file) =>
          renderer.setText(file + " (error: invalid mod)")
          renderer.setForeground(Color.RED)
        case Right(mod) =>
          renderer.setText(mod.name)
      }
      renderer
    }
  })
  modList.setSelectedIndex(0)
  panel.add(modList, BorderLayout.CENTER)
  
  val buttonPanel = new JPanel()
  buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS))
  val selectButton = new JButton("Select")
  selectButton.addActionListener(new ActionListener() {
    def actionPerformed(ev: ActionEvent) = {
      val sand = new TailSand(modList.getSelectedValue.right.get)
      sand.start()
      modsDialog.dispose()
    }
  })
  buttonPanel.add(selectButton)
  
  panel.add(buttonPanel, BorderLayout.SOUTH)
  panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5))
  
  def showDialog() = {
    modsDialog.setContentPane(panel)
    modsDialog.pack()
    val dw = modsDialog.getWidth max 300
    val dh = modsDialog.getHeight max 200
    modsDialog.setSize(dw, dh)
    modsDialog.setLocationRelativeTo(null)
    modsDialog.setVisible(true)
    modsDialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  }
  
}
class TailSand(mod: SandMod) {
  
  private val panel = new JPanel()
  private val sidebar = new JPanel()
  private val game = new SandComponent[mod.Particle](300, 200, mod)
  
  sidebar.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5))
  panel.setLayout(new BorderLayout());
  {
    val aligner = new JPanel()
    val filler = new JPanel()
    aligner.setLayout(new BorderLayout())
    aligner.add(sidebar, BorderLayout.NORTH)
    aligner.add(filler, BorderLayout.CENTER)
    aligner.setBorder(BorderFactory.createEtchedBorder())
    panel.add(aligner, BorderLayout.EAST)
  }
  panel.add(game, BorderLayout.CENTER)
  
  sidebar.setLayout(new BoxLayout(sidebar, BoxLayout.Y_AXIS))
  val brushSizeBox = new JSpinner(new SpinnerNumberModel(3, 1, 100, 1))
  val searchBox = new JTextField("NYI", 10);
  {
    val fields = new JPanel()
    val layout = new GroupLayout(fields)
    val searchLabel = new JLabel("Search:")
    val brushSizeLabel = new JLabel("Brush Size:")
    searchLabel.setAlignmentX(0.0f)
    layout.setAutoCreateGaps(true)
    layout.setVerticalGroup(layout.createSequentialGroup().
        addGroup(layout.createParallelGroup().addComponent(brushSizeLabel).addComponent(brushSizeBox)).
        addGroup(layout.createParallelGroup().addComponent(searchLabel).addComponent(searchBox)))
    layout.setHorizontalGroup(layout.createSequentialGroup().
        addGroup(layout.createParallelGroup().addComponent(brushSizeLabel).addComponent(searchLabel)).
        addGroup(layout.createParallelGroup().addComponent(brushSizeBox).addComponent(searchBox)))
    fields.setLayout(layout)
    sidebar.add(fields)
  }
  
  private val tags = new JList(mod.tags.keys.toArray.sorted)
  tags.setBorder(BorderFactory.createEtchedBorder())
  private val elems = new JList[mod.Particle]()
  elems.setBorder(BorderFactory.createEtchedBorder());
  {
    val parts = new JPanel()
    parts.setLayout(new GridLayout(1, 2, 5, 5))
    parts.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0))
    parts.add(tags)
    parts.add(elems)
    sidebar.add(parts)
  }
  elems.setCellRenderer(new ListCellRenderer[mod.Particle]() {
    val renderer = new DefaultListCellRenderer()
    def getListCellRendererComponent(list: JList[_ <: mod.Particle], value: mod.Particle, index: Int, isSelected: Boolean, cellHasFocus: Boolean) = {
      renderer.asInstanceOf[ListCellRenderer[Object]].getListCellRendererComponent(list, value, index, isSelected, cellHasFocus)
      renderer.setText(value.asInstanceOf[mod.Particle].name)
      renderer
    }
  })
  tags.addListSelectionListener(new ListSelectionListener() {
    def valueChanged(ev: ListSelectionEvent) = {
      elems.setListData(mod.tags(tags.getSelectedValue).toArray[Object with mod.Particle])
    }
  })
  tags.setSelectedIndex(0)
  private val description = new JTextArea()
  description.setEditable(false)
  description.setLineWrap(true)
  description.setWrapStyleWord(true)
  description.setRows(4);
  {
    val pane = new JScrollPane(description)
    sidebar.add(pane)
  }
  elems.addListSelectionListener(new ListSelectionListener() {
    def valueChanged(ev: ListSelectionEvent) = {
      if (elems.getSelectedValue != null) {
        description.setText(elems.getSelectedValue.description)
        game.brush = elems.getSelectedValue
      }
      else {
        description.setText("")
      }
    }
  })
  brushSizeBox.addChangeListener(new ChangeListener() {
    def stateChanged(ev: ChangeEvent) = {
      game.brushSize = brushSizeBox.getValue.asInstanceOf[Integer] - 1
    }
  })
  
  val fpsLabel = new JLabel("FPS: ???")
  panel.add(fpsLabel, BorderLayout.SOUTH)
  
  def start() = {
    val frame = new JFrame(s"TailSand - ${mod.name}")
    frame.setContentPane(panel)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.pack()
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
    val gameThread = new Thread(new Runnable() {
      def run() = {
        val second = 1000000000
        var t0 = System.nanoTime()
        var dt = 1/60.0
        var tick = 0
        while (frame.isVisible()) {
          game.input()
          game.step()
          game.render()
          val t1 = System.nanoTime()
          val sleep = (second/60) - (t1 - t0)
          Thread.sleep(sleep / 1000000 max 1)
          val t2 = System.nanoTime()
          dt = 0.95 * dt + 0.05 * (t2 - t0) / 1000000000.0
          tick += 1
          fpsLabel.setText(s"FPS: ${(10/dt).round / 10.0}")
          t0 = t2
        }
      }
    })
    gameThread.start()
  }
  
}