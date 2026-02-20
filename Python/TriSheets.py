# GNU TERRY PRATCHETT | ><^
# TM JESSE HASKIN 2026 | DODECAGONE SYSTEMS
# FORGE: AUTO-SCALING TRI-SHEET V3.1
# ------------------------------------------
import sys
import math
from PyQt6.QtWidgets import (QApplication, QGraphicsView, QGraphicsScene, 
                             QGraphicsPolygonItem, QMainWindow, QWidget, 
                             QVBoxLayout, QInputDialog, QGraphicsTextItem)
from PyQt6.QtGui import QPolygonF, QColor, QPen, QBrush, QFont, QTextOption
from PyQt6.QtCore import QPointF, Qt, QRectF

class TriCell(QGraphicsPolygonItem):
    def __init__(self, points, r, c, scene):
        super().__init__(QPolygonF(points))
        self.setAcceptHoverEvents(True)
        self.setBrush(QBrush(QColor("#ffffff")))
        self.setPen(QPen(QColor("#000000"), 1))
        self.points = points
        self.r, self.c = r, c
        self.scene_ref = scene
        self.text_item = None
        self.data = ""

    def mousePressEvent(self, event):
        text, ok = QInputDialog.getMultiLineText(None, f"Cell {self.r},{self.c}", "Enter Shard Data:", self.data)
        if ok:
            self.update_data(text)

    def update_data(self, text):
        self.data = text
        if self.text_item:
            self.scene_ref.removeItem(self.text_item)
        
        if not text: return

        self.text_item = QGraphicsTextItem(self.data)
        self.text_item.setDefaultTextColor(QColor("#000000"))
        
        # Determine if triangle is Up or Down
        is_up = self.points[2].y() < self.points[0].y()
        
        # Recursive scaling logic
        font_size = 20
        while font_size > 4:
            font = QFont("Courier", font_size, QFont.Weight.Bold)
            self.text_item.setFont(font)
            
            # Word wrap settings
            opt = QTextOption()
            opt.setWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)
            opt.setAlignment(Qt.AlignmentFlag.AlignCenter)
            self.text_item.document().setDefaultTextOption(opt)
            self.text_item.setTextWidth(self.boundingRect().width() * 0.7)
            
            # Check if text fits inside the polygon
            if self.contains_text(self.text_item.boundingRect()):
                break
            font_size -= 1

        # Center the text
        t_rect = self.text_item.boundingRect()
        c_rect = self.boundingRect()
        self.text_item.setPos(c_rect.center().x() - t_rect.width()/2, 
                             c_rect.center().y() - t_rect.height()/2 + (5 if is_up else -5))
        self.scene_ref.addItem(self.text_item)

    def contains_text(self, text_rect):
        # Offset and check 4 corners of text relative to triangle
        c_rect = self.boundingRect()
        t_pos = c_rect.center() - text_rect.center()
        shifted_rect = text_rect.translated(t_pos)
        
        corners = [shifted_rect.topLeft(), shifted_rect.topRight(), 
                   shifted_rect.bottomLeft(), shifted_rect.bottomRight()]
        poly = self.polygon()
        return all(poly.containsPoint(p, Qt.FillRule.OddEvenFill) for p in corners)

    def hoverEnterEvent(self, event):
        self.setBrush(QBrush(QColor("#ffff00")))

    def hoverLeaveEvent(self, event):
        self.setBrush(QBrush(QColor("#ffffff")))

class TriWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("DodecaGone Tri-Sheet v3.1 [AUTO-SCALE]")
        self.resize(1000, 750)
        self.setStyleSheet("QMainWindow { background-color: #008080; } QGraphicsView { background-color: #c0c0c0; border: 4px outset white; }")
        
        container = QWidget()
        layout = QVBoxLayout(container)
        self.scene = QGraphicsScene()
        self.view = QGraphicsView(self.scene)
        layout.addWidget(self.view)
        self.setCentralWidget(container)
        self.draw_grid(10, 15, 120)

    def draw_grid(self, rows, cols, size):
        h = size * math.sqrt(3) / 2
        for r in range(rows):
            for c in range(cols):
                x, y = c * (size / 2), r * h
                if (r + c) % 2 == 0:
                    p = [QPointF(x, y+h), QPointF(x+size, y+h), QPointF(x+size/2, y)]
                else:
                    p = [QPointF(x, y), QPointF(x+size, y), QPointF(x+size/2, y+h)]
                self.scene.addItem(TriCell(p, r, c, self.scene))

if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = TriWindow(); w.show()
    sys.exit(app.exec())
