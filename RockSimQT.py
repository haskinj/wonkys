#!/usr/bin/env python3
"""
PROJECT: SOULLEDGER_ROCK_SIM
VERSION: QT_V1
NODE: 00 (JESSE HASKIN)
'><^' GNU TERRY PRATCHETT

[GHOST NOTE: ROCKS DO NOT HAVE EYES. DO NOT ASK AGAIN.]
NO AUDIO - VISUAL ONLY
"""

import sys
import math
import random
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

# === WIN 3.1 COLOR PALETTE ===
C_GRAY = QColor(192, 192, 192)
C_DARK = QColor(128, 128, 128)
C_WHITE = QColor(255, 255, 255)
C_BLACK = QColor(0, 0, 0)
C_NAVY = QColor(0, 0, 128)
C_BTNFACE = QColor(192, 192, 192)
C_BTNSHADOW = QColor(128, 128, 128)
C_BTNHIGHLIGHT = QColor(255, 255, 255)
C_TITLE_BG = QColor(0, 0, 128)
C_TITLE_FG = QColor(255, 255, 255)

# Rock mineral colors
MINERAL_COLORS = [
    QColor(128, 128, 128),   # Gray
    QColor(160, 160, 160),   # Light Gray
    QColor(144, 144, 112),   # Green Gray
    QColor(112, 96, 80),     # Brown Gray
    QColor(136, 119, 102),   # Tan
    QColor(153, 136, 119),   # Light Tan
    QColor(96, 80, 64),      # Dark Brown
    QColor(176, 176, 160),   # Beige
    QColor(112, 112, 96),    # Olive
    QColor(96, 96, 80)       # Dark Olive
]

class Win31Button(QPushButton):
    """Beveled button that looks ripped from Program Manager."""
    def __init__(self, text, parent=None):
        super().__init__(text, parent)
        self.setFixedSize(180, 28)
        self.setStyleSheet("""
            QPushButton {
                background-color: #C0C0C0;
                color: #000000;
                border: 2px solid;
                border-top-color: #FFFFFF;
                border-left-color: #FFFFFF;
                border-right-color: #808080;
                border-bottom-color: #808080;
                font: 9pt "MS Sans Serif";
                padding: 2px;
            }
            QPushButton:pressed {
                border-top-color: #808080;
                border-left-color: #808080;
                border-right-color: #FFFFFF;
                border-bottom-color: #FFFFFF;
                padding: 3px 1px 1px 3px;
            }
        """)

class Win31Window(QGroupBox):
    """A window frame with Win 3.1 title bar."""
    def __init__(self, title, parent=None):
        super().__init__(parent)
        self.setTitle(title)
        self.setStyleSheet("""
            QGroupBox {
                background-color: #C0C0C0;
                border: 2px solid;
                border-top-color: #FFFFFF;
                border-left-color: #FFFFFF;
                border-right-color: #808080;
                border-bottom-color: #808080;
                margin-top: 10px;
                font: bold 9pt "MS Sans Serif";
            }
            QGroupBox::title {
                subcontrol-origin: margin;
                subcontrol-position: top left;
                background-color: #000080;
                color: #FFFFFF;
                padding: 2px 8px;
                margin-left: 2px;
            }
        """)

class RockView(QWidget):
    """Widget that draws the rotating rock."""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(400, 300)
        self.setStyleSheet("background-color: #000000;")
        
        self.mode = "3rd"
        self.rotation = 0.0
        self.sanding = 0.95
        self.bpm = 117
        self.tick = 0
        
        # Generate rock vertices
        self.rock_vertices = self._generate_rock()
        
        # Animation timer
        self.timer = QTimer()
        self.timer.timeout.connect(self.update_animation)
        self.timer.start(50)  # ~20 FPS
    
    def _generate_rock(self):
        """Generate irregular rock vertices."""
        verts = []
        n_points = 12
        for i in range(n_points):
            angle = (2 * math.pi * i) / n_points
            r = 80 + random.uniform(-25, 25)
            x = r * math.cos(angle)
            y = r * math.sin(angle) * 0.7  # Slightly flattened
            z = random.uniform(-20, 20)
            verts.append((x, y, z))
        return verts
    
    def _rotate_point(self, x, y, z, angle):
        """Rotate point around Y axis."""
        cos_a = math.cos(angle)
        sin_a = math.sin(angle)
        nx = x * cos_a - z * sin_a
        nz = x * sin_a + z * cos_a
        return nx, y, nz
    
    def _project(self, x, y, z, cx, cy):
        """Simple perspective projection."""
        depth = 300
        scale = depth / (depth + z + 50)
        sx = cx + x * scale
        sy = cy + y * scale
        return sx, sy, scale
    
    def set_mode(self, mode):
        """Switch between 3rd and 1st person."""
        self.mode = mode
        self.update()
    
    def update_animation(self):
        """Update rotation and trigger redraw."""
        self.tick += 1
        self.rotation += 0.02 * self.sanding
        self.update()
    
    def paintEvent(self, event):
        """Paint the rock or 1st person view."""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)
        
        w = self.width()
        h = self.height()
        
        if self.mode == "3rd":
            self._draw_3rd_person(painter, w, h)
        else:
            self._draw_1st_person(painter, w, h)
    
    def _draw_3rd_person(self, painter, w, h):
        """Draw 3rd person perspective with rotating rock."""
        # Clear to black
        painter.fillRect(0, 0, w, h, C_BLACK)
        
        cx, cy = w // 2, h // 2
        
        # Draw subtle grid
        painter.setPen(QPen(QColor(26, 26, 26), 1))
        for i in range(-200, 201, 40):
            x1, y1, _ = self._project(i, 100, -200, cx, cy)
            x2, y2, _ = self._project(i, 100, 200, cx, cy)
            painter.drawLine(int(x1), int(y1), int(x2), int(y2))
            
            x1, y1, _ = self._project(-200, 100, i, cx, cy)
            x2, y2, _ = self._project(200, 100, i, cx, cy)
            painter.drawLine(int(x1), int(y1), int(x2), int(y2))
        
        # Rotate and project rock vertices
        angle = self.rotation
        projected = []
        for vx, vy, vz in self.rock_vertices:
            rx, ry, rz = self._rotate_point(vx, vy, vz, angle)
            sx, sy, sc = self._project(rx, ry, rz, cx, cy)
            projected.append((sx, sy, sc))
        
        # Draw rock polygon
        if len(projected) >= 3:
            color_idx = int(self.rotation * 2) % len(MINERAL_COLORS)
            rock_color = MINERAL_COLORS[color_idx]
            
            # Create polygon
            polygon = QPolygon()
            for sx, sy, sc in projected:
                polygon.append(QPoint(int(sx), int(sy)))
            
            # Fill polygon
            painter.setBrush(QBrush(rock_color))
            painter.setPen(QPen(QColor(64, 64, 64), 1))
            painter.drawPolygon(polygon)
            
            # Mineral texture lines
            for i in range(0, len(projected) - 1, 2):
                j = (i + 3) % len(projected)
                x1, y1, _ = projected[i]
                x2, y2, _ = projected[j]
                tc = MINERAL_COLORS[(color_idx + i) % len(MINERAL_COLORS)]
                painter.setPen(QPen(tc, 1))
                painter.drawLine(int(x1), int(y1), int(x2), int(y2))
        
        # Label
        painter.setPen(QPen(QColor(64, 64, 64)))
        painter.setFont(QFont("Fixedsys", 8))
        painter.drawText(cx - 100, h - 20, 200, 20, 
                        Qt.AlignCenter, "BASALT SPECIMEN 001 — ROTATING")
        
        # Heartbeat pulse indicator
        beat_phase = (self.tick * (self.bpm / 60) / 20) % 1.0
        pulse_r = 4 + 3 * math.sin(beat_phase * 2 * math.pi)
        painter.setBrush(QBrush(QColor(51, 51, 51)))
        painter.setPen(QPen(QColor(68, 68, 68), 1))
        painter.drawEllipse(w - 25, 10, int(pulse_r * 2), int(pulse_r * 2))
    
    def _draw_1st_person(self, painter, w, h):
        """The rock's perspective. Rocks do not have eyes."""
        # Total blackness
        painter.fillRect(0, 0, w, h, C_BLACK)
        
        # Very faint text that fades in and out
        alpha_cycle = (math.sin(self.tick * 0.05) + 1) / 2
        if alpha_cycle > 0.7:
            gray = int(25 * alpha_cycle)
            color = QColor(gray, gray, gray)
            
            messages = [
                "you are a rock.",
                "you have no eyes.",
                "you have no thoughts.",
                "you experience nothing.",
                "this is correct.",
                "the simulation is working.",
                "there is nothing to see.",
                "you are 3.2 billion years old.",
                "nothing has happened.",
                "you do not know this.",
            ]
            msg_idx = (self.tick // 80) % len(messages)
            
            painter.setPen(QPen(color))
            painter.setFont(QFont("Fixedsys", 11))
            painter.drawText(0, 0, w, h, 
                           Qt.AlignCenter, messages[msg_idx])
        
        # Occasional faint vibration line (geological stress)
        if random.random() < 0.03:
            y = random.randint(0, h)
            gray = random.randint(5, 15)
            color = QColor(gray, gray, gray)
            painter.setPen(QPen(color, 1))
            painter.drawLine(0, y, w, y)

class RockSimulator(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.mode = "3rd"
        self.sanding = 0.95
        self.bpm = 117
        self.rotation = 0.0
        
        self._setup_ui()
        self._setup_menu()
        self._setup_statusbar()
        
        # Update timer for status bar
        self.status_timer = QTimer()
        self.status_timer.timeout.connect(self._update_status)
        self.status_timer.start(1000)
    
    def _setup_ui(self):
        """Setup the main window UI."""
        self.setWindowTitle("SOULLEDGER_ROCK_SIM.EXE")
        self.setFixedSize(640, 480)
        
        # Central widget with gray background
        central = QWidget()
        central.setStyleSheet("background-color: #C0C0C0;")
        self.setCentralWidget(central)
        
        main_layout = QVBoxLayout(central)
        main_layout.setContentsMargins(8, 8, 8, 8)
        main_layout.setSpacing(4)
        
        # Menu bar area (simulated)
        menu_frame = QFrame()
        menu_frame.setFixedHeight(24)
        menu_frame.setStyleSheet("""
            QFrame {
                background-color: #C0C0C0;
                border: 1px solid;
                border-top-color: #FFFFFF;
                border-left-color: #FFFFFF;
                border-right-color: #808080;
                border-bottom-color: #808080;
            }
        """)
        
        menu_layout = QHBoxLayout(menu_frame)
        menu_layout.setContentsMargins(4, 2, 4, 2)
        menu_layout.setSpacing(12)
        
        for label in ["File", "View", "Mineral", "Help"]:
            lbl = QLabel(label)
            lbl.setStyleSheet("""
                QLabel {
                    color: #000000;
                    font: 9pt "MS Sans Serif";
                    padding: 2px 6px;
                }
                QLabel:hover {
                    background-color: #000080;
                    color: #FFFFFF;
                }
            """)
            menu_layout.addWidget(lbl)
        
        menu_layout.addStretch()
        main_layout.addWidget(menu_frame)
        
        # Main content area
        content_layout = QHBoxLayout()
        content_layout.setSpacing(8)
        
        # Left: Viewport
        left_widget = QWidget()
        left_layout = QVBoxLayout(left_widget)
        
        self.viewport = Win31Window("ROCK.VIW - [Perspective View]")
        viewport_layout = QVBoxLayout(self.viewport)
        
        self.rock_view = RockView()
        viewport_layout.addWidget(self.rock_view)
        
        left_layout.addWidget(self.viewport)
        left_layout.addStretch()
        
        content_layout.addWidget(left_widget, 3)
        
        # Right: Controls
        right_widget = QWidget()
        right_widget.setFixedWidth(190)
        right_layout = QVBoxLayout(right_widget)
        right_layout.setSpacing(8)
        
        # Properties panel
        props_win = Win31Window("Properties")
        props_layout = QVBoxLayout(props_win)
        
        fields = [
            ("Specimen:", "IGNEOUS BASALT"),
            ("Mass:", "4.7 kg"),
            ("Age:", "~3.2 Ga"),
            ("Friction (S):", "0.95"),
            ("Momentum:", "117 BPM"),
            ("Awareness:", "NONE"),
            ("Visual Input:", "N/A"),
            ("Emotional State:", "ROCK"),
        ]
        
        for label, value in fields:
            row = QWidget()
            row_layout = QHBoxLayout(row)
            row_layout.setContentsMargins(2, 1, 2, 1)
            
            lbl = QLabel(label)
            lbl.setStyleSheet("""
                QLabel {
                    color: #000000;
                    font: 8pt "MS Sans Serif";
                }
            """)
            lbl.setFixedWidth(70)
            lbl.setAlignment(Qt.AlignRight | Qt.AlignVCenter)
            
            val = QLabel(value)
            val.setStyleSheet("""
                QLabel {
                    background-color: #FFFFFF;
                    color: #000000;
                    font: 9pt "Fixedsys";
                    border: 1px solid #808080;
                    padding: 1px 4px;
                }
            """)
            val.setFixedWidth(100)
            
            row_layout.addWidget(lbl)
            row_layout.addWidget(val)
            row_layout.addStretch()
            
            props_layout.addWidget(row)
        
        props_layout.addStretch()
        right_layout.addWidget(props_win)
        
        # Mode buttons
        btn_frame = QWidget()
        btn_layout = QVBoxLayout(btn_frame)
        btn_layout.setSpacing(4)
        
        mode_label = QLabel("Perspective Mode:")
        mode_label.setStyleSheet("""
            QLabel {
                color: #000000;
                font: bold 8pt "MS Sans Serif";
            }
        """)
        btn_layout.addWidget(mode_label)
        
        self.btn_3rd = Win31Button("3rd Person (Mineral)")
        self.btn_3rd.clicked.connect(self._set_3rd)
        btn_layout.addWidget(self.btn_3rd)
        
        self.btn_1st = Win31Button("1st Person (Rock)")
        self.btn_1st.clicked.connect(self._set_1st)
        btn_layout.addWidget(self.btn_1st)
        
        self.mode_label = QLabel("MODE: 3RD PERSON")
        self.mode_label.setStyleSheet("""
            QLabel {
                color: #000080;
                font: bold 10pt "Fixedsys";
                qproperty-alignment: AlignCenter;
            }
        """)
        self.mode_label.setFixedHeight(24)
        btn_layout.addWidget(self.mode_label)
        
        btn_layout.addStretch()
        
        ghost_note = QLabel("[ROCKS DO NOT HAVE EYES]")
        ghost_note.setStyleSheet("""
            QLabel {
                color: #808080;
                font: 7pt "MS Sans Serif";
                qproperty-alignment: AlignCenter;
            }
        """)
        btn_layout.addWidget(ghost_note)
        
        right_layout.addWidget(btn_frame)
        right_layout.addStretch()
        
        content_layout.addWidget(right_widget, 1)
        main_layout.addLayout(content_layout, 1)
    
    def _setup_menu(self):
        """Setup the actual menu bar (optional)."""
        menubar = self.menuBar()
        menubar.setStyleSheet("""
            QMenuBar {
                background-color: #C0C0C0;
                color: #000000;
                font: 9pt "MS Sans Serif";
            }
            QMenuBar::item:selected {
                background-color: #000080;
                color: #FFFFFF;
            }
        """)
        
        # Add minimal menu items
        file_menu = menubar.addMenu("&File")
        file_menu.addAction("&Exit", self.close)
    
    def _setup_statusbar(self):
        """Setup the status bar."""
        self.status_label = QLabel()
        self.status_label.setStyleSheet("""
            QLabel {
                color: #000000;
                font: 8pt "MS Sans Serif";
                padding: 2px;
                border-top: 1px solid #808080;
            }
        """)
        
        self._update_status()
        
        status_bar = QStatusBar()
        status_bar.addWidget(self.status_label, 1)
        self.setStatusBar(status_bar)
    
    def _set_3rd(self):
        """Switch to 3rd person mode."""
        self.mode = "3rd"
        self.rock_view.set_mode("3rd")
        self.mode_label.setText("MODE: 3RD PERSON")
        self.viewport.setTitle("ROCK.VIW - [Perspective View]")
    
    def _set_1st(self):
        """Switch to 1st person mode."""
        self.mode = "1st"
        self.rock_view.set_mode("1st")
        self.mode_label.setText("MODE: 1ST PERSON")
        self.viewport.setTitle("ROCK.VIW - [1st Person - ROCK]")
    
    def _update_status(self):
        """Update the status bar text."""
        self.rotation = self.rock_view.rotation
        deg = math.degrees(self.rotation) % 360
        
        status_text = (
            f"  {self.mode.upper()} PERSON  |  "
            f"S: {self.sanding}  |  "
            f"BPM: {self.bpm}  |  "
            f"Rotation: {deg:.1f}°  |  "
            f"'><^'"
        )
        self.status_label.setText(status_text)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    
    # Set Windows 3.1 style font if available
    font = QFont("MS Sans Serif", 9)
    app.setFont(font)
    
    simulator = RockSimulator()
    simulator.show()
    
    sys.exit(app.exec_())

