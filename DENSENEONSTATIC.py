#!/usr/bin/env python3
"""
DENSE NEON STATIC - HIGH DENSITY, SMALL WINDOW
"""

import tkinter as tk
import random
import time

class DenseNeonStatic:
    def __init__(self, width=400, height=300):
        self.root = tk.Tk()
        self.root.title("DENSE NEON STATIC")
        self.root.configure(bg='black')
        
        # Smaller window
        self.width = width
        self.height = height
        self.root.geometry(f"{width}x{height}+100+100")
        
        # No window decorations for cleaner look
        self.root.overrideredirect(True)
        
        # Create canvas
        self.canvas = tk.Canvas(self.root, bg='#000011', highlightthickness=0)
        self.canvas.pack(fill=tk.BOTH, expand=True)
        
        # NEON COLOR PALETTE - MORE SATURATED
        self.neon_colors = [
            '#FF00FF',  # MAGENTA
            '#00FFFF',  # CYAN  
            '#00FF00',  # ACID GREEN
            '#FFFF00',  # YELLOW
            '#FF0055',  # HOT PINK
            '#00FF88',  # MINT
            '#AA00FF',  # PURPLE
            '#FF5500',  # ORANGE
            '#55FF00',  # LIME
            '#FF00AA',  # DEEP PINK
        ]
        
        # Bind keys
        self.root.bind('<Escape>', self.close)
        self.root.bind('<Button-1>', self.close)
        self.root.bind('<space>', self.close)
        
        # Start animation
        self.running = True
        self.last_time = time.time()
        self.frame_count = 0
        
        # Start with initial draw
        self.root.after(10, self.draw_static)
        
    def draw_static(self):
        """Draw high-density neon static"""
        if not self.running:
            return
            
        # Clear canvas but leave some trails for denser effect
        self.canvas.delete("static")
        
        # Get current dimensions
        width = self.canvas.winfo_width()
        height = self.canvas.winfo_height()
        
        if width <= 1 or height <= 1:
            self.root.after(10, self.draw_static)
            return
        
        # HIGH DENSITY - More pixels per frame
        for _ in range(1500):  # Increased from 500
            x = random.randint(0, width)
            y = random.randint(0, height)
            
            # Smaller, more dense pixels
            size = random.randint(1, 3)  # Smaller size
            
            # More color variety with weighting
            color_choice = random.random()
            if color_choice < 0.3:  # 30% magenta
                color = self.neon_colors[0]
            elif color_choice < 0.5:  # 20% cyan
                color = self.neon_colors[1]
            elif color_choice < 0.7:  # 20% green
                color = self.neon_colors[2]
            else:  # 30% random other neon
                color = random.choice(self.neon_colors)
            
            # Draw with tag for easy clearing
            self.canvas.create_rectangle(
                x, y, x + size, y + size,
                fill=color, outline=color, width=0,
                tags="static"
            )
        
        # Add some larger "glitch" rectangles occasionally
        if random.random() < 0.3:  # 30% chance per frame
            for _ in range(5):
                x = random.randint(0, width)
                y = random.randint(0, height)
                w = random.randint(10, 30)
                h = random.randint(10, 30)
                color = random.choice(self.neon_colors)
                self.canvas.create_rectangle(
                    x, y, x + w, y + h,
                    fill=color, outline=color, width=0,
                    tags="static"
                )
        
        # Faster refresh rate for denser feel
        self.root.after(30, self.draw_static)  # 33 FPS
        
    def close(self, event=None):
        """Close the window"""
        self.running = False
        self.root.destroy()

def main():
    print("=" * 50)
    print("DENSE NEON STATIC GENERATOR")
    print("=" * 50)
    print("Controls:")
    print("  - ESC: Close window")
    print("  - SPACE: Close window")
    print("  - CLICK: Close window")
    print()
    print("Creating 400x300 window with dense neon static...")
    
    # Create the static window
    app = DenseNeonStatic(width=400, height=300)
    
    # Alternative: Uncomment for 800x600 window
    # app = DenseNeonStatic(width=800, height=600)
    
    try:
        app.root.mainloop()
    except:
        pass
    
    print("Window closed.")

if __name__ == "__main__":
    main()