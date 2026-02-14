#!/usr/bin/env python3
"""
Windows 3.1 Style Panic Progress Bar
Progress creeps up. At certain thresholds, despair manifests.
When it reaches 99%, it vanishes, leaving a haunted README.txt.
"""

import sys
import random
from PyQt5.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout,
                             QProgressBar, QLabel, QFrame)
from PyQt5.QtCore import QTimer, Qt
from PyQt5.QtGui import QFont

class PanicProgress(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Program Manager - [Panic Install]")
        self.setGeometry(100, 100, 400, 250)
        self.setFixedSize(400, 250)

        # Set a chunky, retro style
        self.setStyleSheet("""
            QMainWindow {
                background-color: #c0c0c0;
                border: 3px solid #808080;
            }
            QProgressBar {
                border: 2px solid #404040;
                border-radius: 0px;
                background-color: #ffffff;
                height: 25px;
                text-align: center;
                font: bold 10pt 'MS Sans Serif';
            }
            QProgressBar::chunk {
                background-color: #000080;
                width: 10px;
                margin: 1px;
            }
            QLabel {
                color: black;
                background-color: #c0c0c0;
                font: 10pt 'MS Sans Serif';
            }
        """)

        central = QWidget()
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setSpacing(10)

        # Title label
        title = QLabel("Installing Windows 3.1...")
        title.setAlignment(Qt.AlignCenter)
        title.setFont(QFont("MS Sans Serif", 10, QFont.Bold))
        layout.addWidget(title)

        # Progress bar
        self.progress = QProgressBar()
        self.progress.setRange(0, 100)
        self.progress.setValue(0)
        layout.addWidget(self.progress)

        # Main message label (for OH NO escalation)
        self.message = QLabel("")
        self.message.setAlignment(Qt.AlignCenter)
        self.message.setFont(QFont("MS Sans Serif", 10))
        layout.addWidget(self.message)

        # Container for the little "oh no" labels
        self.panic_container = QFrame()
        self.panic_container.setFrameShape(QFrame.Box)
        self.panic_container.setLineWidth(2)
        self.panic_container.setStyleSheet("background-color: #a0a0a0;")
        self.panic_layout = QVBoxLayout(self.panic_container)
        self.panic_layout.setSpacing(2)
        layout.addWidget(self.panic_container)

        # Timer for progress updates
        self.timer = QTimer()
        self.timer.timeout.connect(self.advance_progress)
        self.timer.start(150)  # moderately slow

        # Flash timer for the final stage
        self.flash_timer = QTimer()
        self.flash_timer.timeout.connect(self.flash_message)

        # Track state
        self.panic_level = 0  # 0=normal, 1=85%, 2=90%, 3=95%
        self.flash_state = False

    def advance_progress(self):
        current = self.progress.value()
        if current >= 99:
            self.leave_panic_note()
            self.timer.stop()
            QApplication.quit()
            return

        new_val = current + 1
        self.progress.setValue(new_val)

        # Trigger events based on threshold
        if new_val >= 85 and self.panic_level == 0:
            self.panic_level = 1
            self.add_oh_no_labels()
        if new_val >= 90 and self.panic_level == 1:
            self.panic_level = 2
            self.message.setText("OH NO")
            self.message.setFont(QFont("MS Sans Serif", 14, QFont.Bold))
        if new_val >= 95 and self.panic_level == 2:
            self.panic_level = 3
            self.message.setText("OH NO!!")
            self.message.setFont(QFont("MS Sans Serif", 18, QFont.Bold))
            # Start flashing
            self.flash_timer.start(200)

    def add_oh_no_labels(self):
        # Add several small "oh no" labels to the container
        for _ in range(8):  # a bunch
            label = QLabel("oh no")
            label.setAlignment(Qt.AlignCenter)
            label.setFont(QFont("MS Sans Serif", 8))
            label.setStyleSheet("color: #800000; background-color: #a0a0a0;")
            self.panic_layout.addWidget(label)

    def flash_message(self):
        # Toggle the message label's visibility to create flashing effect
        self.flash_state = not self.flash_state
        if self.flash_state:
            self.message.setStyleSheet("color: #ff0000; background-color: #ffff00;")
        else:
            self.message.setStyleSheet("color: black; background-color: #c0c0c0;")

    def leave_panic_note(self):
        """Create a README.txt full of despair."""
        messages = [
            "OH NO", "not again!", "why why why",
            "oh no", "not again!", "WHY",
            "oh no oh no oh no", "not again!",
            "why", "whywhywhy", "OH NO",
            "not again!", "why why why"
        ]
        with open("README.txt", "w") as f:
            f.write("\n".join(random.choice(messages) for _ in range(50)))

if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = PanicProgress()
    window.show()
    sys.exit(app.exec_())