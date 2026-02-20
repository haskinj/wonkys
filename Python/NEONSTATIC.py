#!/usr/bin/env python3
"""
NEON STATIC WINDOW - GUARANTEED WORKING
"""

import tkinter as tk
import random
import time

# NEON COLOR PALETTE
NEON_COLORS = [
    '#FF00FF',  # Magenta
    '#00FFFF',  # Cyan  
    '#00FF00',  # Acid Green
    '#FFFF00',  # Yellow
    '#FF0055',  # Hot Pink
    '#00FF88',  # Mint
    '#AA00FF',  # Purple
]

class NeonStatic:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("NEON STATIC")
        self.root.configure(bg='black')
        
        # Make window fullscreen or large
        self.root.attributes('-fullscreen', True)
        
        # Or for regular window:
        # self.root.geometry("800x600")
        
        # Create canvas for static
        self.canvas = tk.Canvas(self.root, bg='black', highlightthickness=0)
        self.canvas.pack(fill=tk.BOTH, expand=True)
        
        # Start animation
        self.running = True
        self.root.bind('<Escape>', self.close)
        self.root.protocol("WM_DELETE_WINDOW", self.close)
        
        self.draw_static()
        
    def draw_static(self):
        """Draw random neon pixels"""
        if not self.running:
            return
            
        # Clear canvas
        self.canvas.delete("all")
        
        # Get window size
        width = self.canvas.winfo_width()
        height = self.canvas.winfo_height()
        
        # If window not ready yet, wait
        if width <= 1 or height <= 1:
            self.root.after(100, self.draw_static)
            return
        
        # Draw random neon pixels
        for _ in range(500):  # Number of pixels per frame
            x = random.randint(0, width)
            y = random.randint(0, height)
            
            # Random size and color
            size = random.randint(2, 6)
            color = random.choice(NEON_COLORS)
            
            # Draw rectangle as pixel
            self.canvas.create_rectangle(
                x, y, x + size, y + size,
                fill=color, outline=color, width=0
            )
        
        # Schedule next frame
        self.root.after(50, self.draw_static)  # 20 FPS
        
    def close(self, event=None):
        """Close the window"""
        self.running = False
        self.root.destroy()

if __name__ == "__main__":
    print("Opening neon static window...")
    print("Press ESC to exit")
    app = NeonStatic()
    app.root.mainloop()