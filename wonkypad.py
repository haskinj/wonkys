#!/usr/bin/env python3
"""
WonkyPad v1.0
A text editor that looks like Windows 95 and respects the user.

Features:
  - Ghost indent guides for Python and HTML
  - Auto-closing brackets, parens, quotes
  - Syntax highlighting for Python and HTML
  - Right-click menu with normal stuff AND:
    • Embiggen (ALL CAPS)
    • Smolify (all lowercase)
    • Sarcasm Case (aLtErNaTiNg CaSe)
    • UwU-ify (replace r/l with w, add uwu)
    • Reverse (sdrawkcab)
    • SHOUTING INTO THE VOID (add exclamation marks)
    • Fancy (add ✨ around selection)
    • Pirate (ye olde English-ish)
    • Haunted (z̷̢a̶̡l̵̢g̶̡o̷̢ text)
    • Please (wrap in polite request)

Mason mark: ><^
GNU TERRY PRATCHETT
"""

import tkinter as tk
from tkinter import filedialog, messagebox, font as tkfont
import re
import os
import random

# ══════════════════════════════════════════════
#  COLOR SCHEMES
# ══════════════════════════════════════════════

WIN95_COLORS = {
    'bg': '#C0C0C0',
    'titlebar': '#000080',
    'titlebar_text': '#FFFFFF',
    'button_face': '#C0C0C0',
    'button_highlight': '#FFFFFF',
    'button_shadow': '#808080',
    'button_dark': '#404040',
    'text_bg': '#FFFFFF',
    'text_fg': '#000000',
    'menu_bg': '#C0C0C0',
    'menu_fg': '#000000',
    'status_bg': '#C0C0C0',
    'indent_guide': '#C2C2C2',
    'selection_bg': '#000080',
    'selection_fg': '#FFFFFF',
}

PYTHON_SYNTAX = {
    'keyword': '#0000FF',
    'builtin': '#900090',
    'string': '#008000',
    'comment': '#808080',
    'number': '#FF4500',
    'decorator': '#AA22FF',
    'self': '#9C6500',
    'operator': '#666666',
    'fstring_brace': '#AA0000',
}

HTML_SYNTAX = {
    'tag': '#0000FF',
    'attribute': '#FF0000',
    'attr_value': '#008000',
    'comment': '#808080',
    'entity': '#900090',
    'doctype': '#AA22FF',
}

PYTHON_KEYWORDS = {
    'False', 'None', 'True', 'and', 'as', 'assert', 'async', 'await',
    'break', 'class', 'continue', 'def', 'del', 'elif', 'else', 'except',
    'finally', 'for', 'from', 'global', 'if', 'import', 'in', 'is',
    'lambda', 'nonlocal', 'not', 'or', 'pass', 'raise', 'return',
    'try', 'while', 'with', 'yield',
}

PYTHON_BUILTINS = {
    'print', 'len', 'range', 'int', 'str', 'float', 'list', 'dict',
    'set', 'tuple', 'bool', 'type', 'isinstance', 'enumerate', 'zip',
    'map', 'filter', 'sorted', 'reversed', 'open', 'input', 'super',
    'property', 'staticmethod', 'classmethod', 'abs', 'all', 'any',
    'bin', 'chr', 'dir', 'divmod', 'eval', 'exec', 'format', 'getattr',
    'globals', 'hasattr', 'hash', 'hex', 'id', 'iter', 'locals', 'max',
    'min', 'next', 'oct', 'ord', 'pow', 'repr', 'round', 'setattr',
    'slice', 'sum', 'vars', 'Exception', 'ValueError', 'TypeError',
    'KeyError', 'IndexError', 'AttributeError', 'ImportError',
    'FileNotFoundError', 'RuntimeError', 'StopIteration',
}

AUTO_CLOSE_PAIRS = {
    '(': ')',
    '[': ']',
    '{': '}',
    '"': '"',
    "'": "'",
}

# Zalgo combining characters for Haunted mode
ZALGO_UP = ['\u0300', '\u0301', '\u0302', '\u0303', '\u0304', '\u0305',
            '\u0306', '\u0307', '\u0308', '\u0309', '\u030A', '\u030B',
            '\u030C', '\u030D', '\u030E', '\u030F', '\u0310', '\u0311',
            '\u0312', '\u0313']

ZALGO_DOWN = ['\u0316', '\u0317', '\u0318', '\u0319', '\u031A', '\u031B',
              '\u031C', '\u031D', '\u031E', '\u031F', '\u0320', '\u0321',
              '\u0322', '\u0323', '\u0324', '\u0325', '\u0326', '\u0327',
              '\u0328', '\u0329']


class WonkyPad:
    def __init__(self, root):
        self.root = root
        self.root.title("WonkyPad — Untitled")
        self.root.geometry("900x640")
        self.root.configure(bg=WIN95_COLORS['bg'])

        self.current_file = None
        self.mode = 'python'  # 'python' or 'html'
        self.modified = False
        self.indent_guides_enabled = True
        self.auto_close_enabled = True
        self.highlight_enabled = True

        # ── FONTS ──
        self.code_font = tkfont.Font(family='Consolas', size=11)
        self.ui_font = tkfont.Font(family='Tahoma', size=9)
        self.status_font = tkfont.Font(family='Tahoma', size=8)
        self.title_font = tkfont.Font(family='Tahoma', size=9, weight='bold')
        self.line_num_font = tkfont.Font(family='Consolas', size=9)

        self._build_ui()
        self._bind_events()
        self._update_status()

    # ══════════════════════════════════════════════
    #  UI CONSTRUCTION
    # ══════════════════════════════════════════════

    def _build_ui(self):
        # ── MENU BAR ──
        self.menubar = tk.Menu(self.root, bg=WIN95_COLORS['menu_bg'],
                               fg=WIN95_COLORS['menu_fg'],
                               activebackground=WIN95_COLORS['titlebar'],
                               activeforeground=WIN95_COLORS['titlebar_text'],
                               font=self.ui_font, relief='flat', bd=0)

        # File menu
        file_menu = tk.Menu(self.menubar, tearoff=0, font=self.ui_font,
                            bg=WIN95_COLORS['menu_bg'], fg=WIN95_COLORS['menu_fg'],
                            activebackground=WIN95_COLORS['titlebar'],
                            activeforeground=WIN95_COLORS['titlebar_text'])
        file_menu.add_command(label="New              Ctrl+N", command=self._new_file)
        file_menu.add_command(label="Open...          Ctrl+O", command=self._open_file)
        file_menu.add_command(label="Save             Ctrl+S", command=self._save_file)
        file_menu.add_command(label="Save As...", command=self._save_as)
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self._quit)
        self.menubar.add_cascade(label="File", menu=file_menu)

        # Edit menu
        edit_menu = tk.Menu(self.menubar, tearoff=0, font=self.ui_font,
                            bg=WIN95_COLORS['menu_bg'], fg=WIN95_COLORS['menu_fg'],
                            activebackground=WIN95_COLORS['titlebar'],
                            activeforeground=WIN95_COLORS['titlebar_text'])
        edit_menu.add_command(label="Undo             Ctrl+Z", command=lambda: self.text.edit_undo())
        edit_menu.add_command(label="Redo             Ctrl+Y", command=lambda: self.text.edit_redo())
        edit_menu.add_separator()
        edit_menu.add_command(label="Cut              Ctrl+X", command=lambda: self.text.event_generate("<<Cut>>"))
        edit_menu.add_command(label="Copy             Ctrl+C", command=lambda: self.text.event_generate("<<Copy>>"))
        edit_menu.add_command(label="Paste            Ctrl+V", command=lambda: self.text.event_generate("<<Paste>>"))
        edit_menu.add_separator()
        edit_menu.add_command(label="Select All       Ctrl+A", command=self._select_all)
        self.menubar.add_cascade(label="Edit", menu=edit_menu)

        # Mode menu
        mode_menu = tk.Menu(self.menubar, tearoff=0, font=self.ui_font,
                            bg=WIN95_COLORS['menu_bg'], fg=WIN95_COLORS['menu_fg'],
                            activebackground=WIN95_COLORS['titlebar'],
                            activeforeground=WIN95_COLORS['titlebar_text'])
        mode_menu.add_command(label="🐍 Python Mode", command=lambda: self._set_mode('python'))
        mode_menu.add_command(label="🌐 HTML Mode", command=lambda: self._set_mode('html'))
        mode_menu.add_separator()
        mode_menu.add_command(label="Toggle Indent Guides", command=self._toggle_indent_guides)
        mode_menu.add_command(label="Toggle Auto-Close", command=self._toggle_auto_close)
        mode_menu.add_command(label="Toggle Highlighting", command=self._toggle_highlighting)
        self.menubar.add_cascade(label="Mode", menu=mode_menu)

        # Help menu
        help_menu = tk.Menu(self.menubar, tearoff=0, font=self.ui_font,
                            bg=WIN95_COLORS['menu_bg'], fg=WIN95_COLORS['menu_fg'],
                            activebackground=WIN95_COLORS['titlebar'],
                            activeforeground=WIN95_COLORS['titlebar_text'])
        help_menu.add_command(label="About WonkyPad", command=self._show_about)
        self.menubar.add_cascade(label="Help", menu=help_menu)

        self.root.config(menu=self.menubar)

        # ── TOOLBAR ──
        toolbar = tk.Frame(self.root, bg=WIN95_COLORS['bg'], height=32)
        toolbar.pack(fill='x', padx=2, pady=(2, 0))

        self.mode_label = tk.Label(toolbar, text="🐍 Python",
                                    font=self.ui_font, bg=WIN95_COLORS['bg'],
                                    fg=WIN95_COLORS['menu_fg'],
                                    relief='sunken', bd=1, padx=8, pady=2)
        self.mode_label.pack(side='left', padx=(4, 8))

        for text, cmd in [("New", self._new_file), ("Open", self._open_file),
                          ("Save", self._save_file)]:
            btn = tk.Button(toolbar, text=text, font=self.ui_font,
                           bg=WIN95_COLORS['button_face'], fg='#000000',
                           relief='raised', bd=1, padx=8, pady=1,
                           command=cmd, cursor='hand2')
            btn.pack(side='left', padx=1)

        # ── MAIN EDITOR AREA ──
        editor_frame = tk.Frame(self.root, bg=WIN95_COLORS['bg'])
        editor_frame.pack(fill='both', expand=True, padx=4, pady=4)

        # Line numbers
        self.line_numbers = tk.Canvas(editor_frame, width=45,
                                       bg='#F0F0F0', highlightthickness=0,
                                       bd=0)
        self.line_numbers.pack(side='left', fill='y')

        # Text widget frame with sunken border (Win95 inset)
        text_frame = tk.Frame(editor_frame, bg=WIN95_COLORS['button_shadow'],
                              bd=0)
        text_frame.pack(side='left', fill='both', expand=True)

        # Inner border for 3D inset effect
        text_inner = tk.Frame(text_frame, bg=WIN95_COLORS['text_bg'], bd=0)
        text_inner.pack(fill='both', expand=True, padx=2, pady=2)

        # Scrollbar
        self.scrollbar = tk.Scrollbar(text_inner, orient='vertical',
                                       bg=WIN95_COLORS['bg'],
                                       troughcolor='#E0E0E0',
                                       relief='flat')
        self.scrollbar.pack(side='right', fill='y')

        # THE TEXT WIDGET
        self.text = tk.Text(text_inner,
                            font=self.code_font,
                            bg=WIN95_COLORS['text_bg'],
                            fg=WIN95_COLORS['text_fg'],
                            insertbackground='#000000',
                            selectbackground=WIN95_COLORS['selection_bg'],
                            selectforeground=WIN95_COLORS['selection_fg'],
                            relief='flat',
                            bd=4,
                            undo=True,
                            autoseparators=True,
                            maxundo=-1,
                            wrap='none',
                            tabs=('4c',),
                            yscrollcommand=self._on_scroll)
        self.text.pack(fill='both', expand=True)
        self.scrollbar.config(command=self._on_scrollbar)

        # Horizontal scrollbar
        self.hscrollbar = tk.Scrollbar(text_inner, orient='horizontal',
                                        bg=WIN95_COLORS['bg'],
                                        troughcolor='#E0E0E0',
                                        relief='flat')
        self.hscrollbar.pack(side='bottom', fill='x')
        self.text.config(xscrollcommand=self.hscrollbar.set)
        self.hscrollbar.config(command=self.text.xview)

        # ── STATUS BAR ──
        status_frame = tk.Frame(self.root, bg=WIN95_COLORS['bg'], height=22)
        status_frame.pack(fill='x', side='bottom')

        self.status_left = tk.Label(status_frame, text="Ready",
                                     font=self.status_font,
                                     bg=WIN95_COLORS['status_bg'],
                                     fg='#444444',
                                     anchor='w', relief='sunken', bd=1,
                                     padx=6)
        self.status_left.pack(side='left', fill='x', expand=True, padx=(2, 1), pady=2)

        self.status_pos = tk.Label(status_frame, text="Ln 1, Col 1",
                                    font=self.status_font,
                                    bg=WIN95_COLORS['status_bg'],
                                    fg='#444444',
                                    anchor='center', relief='sunken', bd=1,
                                    padx=6, width=16)
        self.status_pos.pack(side='left', padx=1, pady=2)

        self.status_mode = tk.Label(status_frame, text="Python",
                                     font=self.status_font,
                                     bg=WIN95_COLORS['status_bg'],
                                     fg='#444444',
                                     anchor='center', relief='sunken', bd=1,
                                     padx=6, width=10)
        self.status_mode.pack(side='left', padx=(1, 2), pady=2)

        # ── RIGHT-CLICK CONTEXT MENU ──
        self.context_menu = tk.Menu(self.root, tearoff=0, font=self.ui_font,
                                     bg=WIN95_COLORS['menu_bg'],
                                     fg=WIN95_COLORS['menu_fg'],
                                     activebackground=WIN95_COLORS['titlebar'],
                                     activeforeground=WIN95_COLORS['titlebar_text'])

        self.context_menu.add_command(label="✂ Cut", command=lambda: self.text.event_generate("<<Cut>>"))
        self.context_menu.add_command(label="📋 Copy", command=lambda: self.text.event_generate("<<Copy>>"))
        self.context_menu.add_command(label="📄 Paste", command=lambda: self.text.event_generate("<<Paste>>"))
        self.context_menu.add_separator()
        self.context_menu.add_command(label="Select All", command=self._select_all)
        self.context_menu.add_separator()

        # ── THE WONKY SUBMENU ──
        wonky_menu = tk.Menu(self.context_menu, tearoff=0, font=self.ui_font,
                              bg=WIN95_COLORS['menu_bg'],
                              fg=WIN95_COLORS['menu_fg'],
                              activebackground=WIN95_COLORS['titlebar'],
                              activeforeground=WIN95_COLORS['titlebar_text'])

        wonky_menu.add_command(label="📢 Embiggen (ALL CAPS)", command=lambda: self._transform('embiggen'))
        wonky_menu.add_command(label="🐜 Smolify (all lowercase)", command=lambda: self._transform('smolify'))
        wonky_menu.add_command(label="🤪 Sarcasm Case (aLtErNaTiNg)", command=lambda: self._transform('sarcasm'))
        wonky_menu.add_command(label="🌸 UwU-ify", command=lambda: self._transform('uwu'))
        wonky_menu.add_command(label="🔄 Reverse (sdrawkcab)", command=lambda: self._transform('reverse'))
        wonky_menu.add_command(label="📣 SHOUT INTO THE VOID", command=lambda: self._transform('shout'))
        wonky_menu.add_command(label="✨ Fancy", command=lambda: self._transform('fancy'))
        wonky_menu.add_command(label="🏴‍☠ Pirate Speak", command=lambda: self._transform('pirate'))
        wonky_menu.add_command(label="👻 Haunted (Zalgo)", command=lambda: self._transform('haunted'))
        wonky_menu.add_command(label="🙏 Please (polite wrap)", command=lambda: self._transform('please'))
        wonky_menu.add_command(label="📊 Sanding Scale It", command=lambda: self._transform('sanding'))
        wonky_menu.add_command(label="🧱 Brick Case (Title Case)", command=lambda: self._transform('brick'))
        wonky_menu.add_command(label="🐍 snake_case_it", command=lambda: self._transform('snake'))

        self.context_menu.add_cascade(label="🎨 Wonkify Selection...", menu=wonky_menu)

        # Configure syntax highlight tags
        self._configure_tags()

    # ══════════════════════════════════════════════
    #  EVENT BINDINGS
    # ══════════════════════════════════════════════

    def _bind_events(self):
        self.text.bind('<KeyRelease>', self._on_key_release)
        self.text.bind('<KeyPress>', self._on_key_press)
        self.text.bind('<Button-3>', self._show_context_menu)
        self.text.bind('<Button-1>', lambda e: self._schedule_update())
        self.text.bind('<Configure>', lambda e: self._update_line_numbers())
        self.text.bind('<Return>', self._on_enter)
        self.text.bind('<Tab>', self._on_tab)

        self.root.bind('<Control-n>', lambda e: self._new_file())
        self.root.bind('<Control-o>', lambda e: self._open_file())
        self.root.bind('<Control-s>', lambda e: self._save_file())
        self.root.bind('<Control-a>', lambda e: self._select_all())
        self.root.bind('<Control-Shift-S>', lambda e: self._save_as())

    # ══════════════════════════════════════════════
    #  SYNTAX HIGHLIGHTING
    # ══════════════════════════════════════════════

    def _configure_tags(self):
        # Python tags
        for tag, color in PYTHON_SYNTAX.items():
            self.text.tag_configure(f'py_{tag}', foreground=color)

        # HTML tags
        for tag, color in HTML_SYNTAX.items():
            self.text.tag_configure(f'html_{tag}', foreground=color)

        # Indent guide tag
        self.text.tag_configure('indent_guide',
                                background=WIN95_COLORS['indent_guide'])

    def _highlight_syntax(self):
        if not self.highlight_enabled:
            return

        # Clear all tags
        for tag in self.text.tag_names():
            if tag.startswith(('py_', 'html_')):
                self.text.tag_remove(tag, '1.0', 'end')

        content = self.text.get('1.0', 'end')

        if self.mode == 'python':
            self._highlight_python(content)
        elif self.mode == 'html':
            self._highlight_html(content)

    def _highlight_python(self, content):
        lines = content.split('\n')
        for line_num, line in enumerate(lines, 1):
            offset = 0
            i = 0
            while i < len(line):
                # Comments
                if line[i] == '#':
                    start = f'{line_num}.{i}'
                    end = f'{line_num}.{len(line)}'
                    self.text.tag_add('py_comment', start, end)
                    break

                # Strings (triple-quoted handled simply)
                if line[i] in ('"', "'"):
                    quote = line[i]
                    # Check triple quote
                    if line[i:i+3] in ('"""', "'''"):
                        match = re.search(re.escape(line[i:i+3]), line[i+3:])
                        if match:
                            end_pos = i + 3 + match.end()
                        else:
                            end_pos = len(line)
                        start = f'{line_num}.{i}'
                        end = f'{line_num}.{end_pos}'
                        self.text.tag_add('py_string', start, end)
                        i = end_pos
                        continue
                    else:
                        j = i + 1
                        while j < len(line) and line[j] != quote:
                            if line[j] == '\\':
                                j += 1
                            j += 1
                        j = min(j + 1, len(line))
                        start = f'{line_num}.{i}'
                        end = f'{line_num}.{j}'
                        self.text.tag_add('py_string', start, end)
                        i = j
                        continue

                # Decorators
                if line[i] == '@' and (i == 0 or line[i-1] in ' \t'):
                    match = re.match(r'@\w+', line[i:])
                    if match:
                        start = f'{line_num}.{i}'
                        end = f'{line_num}.{i + match.end()}'
                        self.text.tag_add('py_decorator', start, end)
                        i += match.end()
                        continue

                # Words (keywords, builtins, self)
                if line[i].isalpha() or line[i] == '_':
                    match = re.match(r'\w+', line[i:])
                    if match:
                        word = match.group()
                        start = f'{line_num}.{i}'
                        end = f'{line_num}.{i + len(word)}'

                        if word in PYTHON_KEYWORDS:
                            self.text.tag_add('py_keyword', start, end)
                        elif word in PYTHON_BUILTINS:
                            self.text.tag_add('py_builtin', start, end)
                        elif word == 'self':
                            self.text.tag_add('py_self', start, end)

                        i += len(word)
                        continue

                # Numbers
                if line[i].isdigit():
                    match = re.match(r'\d+\.?\d*', line[i:])
                    if match:
                        start = f'{line_num}.{i}'
                        end = f'{line_num}.{i + match.end()}'
                        self.text.tag_add('py_number', start, end)
                        i += match.end()
                        continue

                i += 1

    def _highlight_html(self, content):
        # HTML tags
        for match in re.finditer(r'</?[\w-]+', content):
            start = self._index_from_offset(match.start())
            end = self._index_from_offset(match.end())
            self.text.tag_add('html_tag', start, end)

        # Closing >
        for match in re.finditer(r'/?>',content):
            start = self._index_from_offset(match.start())
            end = self._index_from_offset(match.end())
            self.text.tag_add('html_tag', start, end)

        # Attributes
        for match in re.finditer(r'[\w-]+=', content):
            start = self._index_from_offset(match.start())
            end = self._index_from_offset(match.end() - 1)
            self.text.tag_add('html_attribute', start, end)

        # Attribute values
        for match in re.finditer(r'="[^"]*"', content):
            start = self._index_from_offset(match.start() + 1)
            end = self._index_from_offset(match.end())
            self.text.tag_add('html_attr_value', start, end)

        # Comments
        for match in re.finditer(r'<!--.*?-->', content, re.DOTALL):
            start = self._index_from_offset(match.start())
            end = self._index_from_offset(match.end())
            self.text.tag_add('html_comment', start, end)

        # Entities
        for match in re.finditer(r'&\w+;', content):
            start = self._index_from_offset(match.start())
            end = self._index_from_offset(match.end())
            self.text.tag_add('html_entity', start, end)

    def _index_from_offset(self, offset):
        """Convert character offset to tk text index."""
        return self.text.index(f'1.0+{offset}c')

    # ══════════════════════════════════════════════
    #  INDENT GUIDES (Ghost Lines)
    # ══════════════════════════════════════════════

    def _update_indent_guides(self):
        self.text.tag_remove('indent_guide', '1.0', 'end')

        if not self.indent_guides_enabled:
            return

        content = self.text.get('1.0', 'end')
        lines = content.split('\n')

        indent_size = 4 if self.mode == 'python' else 2

        for line_num, line in enumerate(lines, 1):
            if not line.strip():
                continue

            # Count leading spaces
            leading = len(line) - len(line.lstrip())
            if leading == 0:
                continue

            # Mark each indent level
            for level in range(indent_size, leading, indent_size):
                start = f'{line_num}.{level - 1}'
                end = f'{line_num}.{level}'
                # Only mark if it's a space
                char = self.text.get(start, end)
                if char == ' ':
                    self.text.tag_add('indent_guide', start, end)

    # ══════════════════════════════════════════════
    #  AUTO-CLOSE BRACKETS
    # ══════════════════════════════════════════════

    def _on_key_press(self, event):
        if not self.auto_close_enabled:
            return

        char = event.char

        # Auto-close pairs
        if char in AUTO_CLOSE_PAIRS:
            # Don't auto-close quotes if we're inside a word
            if char in ('"', "'"):
                pos = self.text.index('insert')
                before = self.text.get(f'{pos}-1c', pos)
                if before.isalnum():
                    return

            closing = AUTO_CLOSE_PAIRS[char]
            self.text.insert('insert', char + closing)
            self.text.mark_set('insert', 'insert-1c')
            self._schedule_update()
            return 'break'

        # Skip over closing bracket if already there
        if char in AUTO_CLOSE_PAIRS.values():
            next_char = self.text.get('insert', 'insert+1c')
            if next_char == char:
                self.text.mark_set('insert', 'insert+1c')
                self._schedule_update()
                return 'break'

    def _on_enter(self, event):
        """Handle enter key with auto-indent."""
        pos = self.text.index('insert')
        line = self.text.get(f'{pos} linestart', pos)

        # Get current indent
        indent = ''
        for ch in line:
            if ch in (' ', '\t'):
                indent += ch
            else:
                break

        # Increase indent after colon (Python)
        if self.mode == 'python' and line.rstrip().endswith(':'):
            indent += '    '

        # Increase indent after opening tags (HTML)
        if self.mode == 'html':
            if re.search(r'<(?!br|hr|img|input|link|meta)[a-zA-Z][^/]*>\s*$', line):
                indent += '  '

        self.text.insert('insert', '\n' + indent)
        self._schedule_update()
        return 'break'

    def _on_tab(self, event):
        """Insert spaces instead of tab."""
        spaces = 4 if self.mode == 'python' else 2
        self.text.insert('insert', ' ' * spaces)
        self._schedule_update()
        return 'break'

    # ══════════════════════════════════════════════
    #  TEXT TRANSFORMS (THE WONKY STUFF)
    # ══════════════════════════════════════════════

    def _transform(self, transform_type):
        try:
            sel_start = self.text.index('sel.first')
            sel_end = self.text.index('sel.last')
            selected = self.text.get(sel_start, sel_end)
        except tk.TclError:
            self.status_left.config(text="Select some text first!")
            return

        result = selected

        if transform_type == 'embiggen':
            result = selected.upper()

        elif transform_type == 'smolify':
            result = selected.lower()

        elif transform_type == 'sarcasm':
            result = ''.join(
                c.upper() if i % 2 else c.lower()
                for i, c in enumerate(selected)
            )

        elif transform_type == 'uwu':
            result = selected.replace('r', 'w').replace('l', 'w')
            result = result.replace('R', 'W').replace('L', 'W')
            result = result.replace('th', 'dw').replace('Th', 'Dw')
            if not result.endswith(' uwu'):
                result += ' uwu'

        elif transform_type == 'reverse':
            result = selected[::-1]

        elif transform_type == 'shout':
            words = selected.upper().split()
            result = '! '.join(words) + '!!!'

        elif transform_type == 'fancy':
            result = f'✨ {selected} ✨'

        elif transform_type == 'pirate':
            result = selected
            replacements = [
                ('my', 'me'), ('is', 'be'), ('are', 'be'),
                ('you', 'ye'), ('your', 'yer'), ('hello', 'ahoy'),
                ('friend', 'matey'), ('the', "th'"),
                ('ing ', "in' "), ('for', 'fer'),
            ]
            for old, new in replacements:
                result = result.replace(old, new)
                result = result.replace(old.capitalize(), new.capitalize())
            if not result.rstrip().endswith('arr'):
                result = result.rstrip() + ', arr!'

        elif transform_type == 'haunted':
            chars = []
            for c in selected:
                chars.append(c)
                num_up = random.randint(1, 3)
                num_down = random.randint(1, 3)
                for _ in range(num_up):
                    chars.append(random.choice(ZALGO_UP))
                for _ in range(num_down):
                    chars.append(random.choice(ZALGO_DOWN))
            result = ''.join(chars)

        elif transform_type == 'please':
            result = f'Could you please {selected.lstrip().lower()}? Thank you.'

        elif transform_type == 'sanding':
            word_count = len(selected.split())
            if word_count < 3:
                level = '1.5 — Manageable'
            elif word_count < 8:
                level = '3.89 — Baseline operational'
            elif word_count < 15:
                level = '6.0 — Getting sandy'
            elif word_count < 25:
                level = '8.5 — Significant friction'
            else:
                level = '11.0 — Emergency. The text is too long.'
            result = f'[Sanding Level: {level}]\n{selected}'

        elif transform_type == 'brick':
            result = selected.title()

        elif transform_type == 'snake':
            # Convert to snake_case
            result = re.sub(r'[\s\-]+', '_', selected.strip())
            result = re.sub(r'([a-z])([A-Z])', r'\1_\2', result)
            result = result.lower()

        self.text.delete(sel_start, sel_end)
        self.text.insert(sel_start, result)
        self._schedule_update()
        self.status_left.config(text=f"Wonkified: {transform_type}")

    # ══════════════════════════════════════════════
    #  LINE NUMBERS
    # ══════════════════════════════════════════════

    def _update_line_numbers(self):
        self.line_numbers.delete('all')

        i = self.text.index('@0,0')
        while True:
            dline = self.text.dlineinfo(i)
            if dline is None:
                break

            y = dline[1]
            linenum = str(i).split('.')[0]
            self.line_numbers.create_text(
                40, y + 2, anchor='ne', text=linenum,
                font=self.line_num_font, fill='#999999'
            )

            i = self.text.index(f'{i}+1line')
            if int(i.split('.')[0]) > int(self.text.index('end').split('.')[0]):
                break

    # ══════════════════════════════════════════════
    #  SCROLLING SYNC
    # ══════════════════════════════════════════════

    def _on_scroll(self, *args):
        self.scrollbar.set(*args)
        self._update_line_numbers()

    def _on_scrollbar(self, *args):
        self.text.yview(*args)
        self._update_line_numbers()

    # ══════════════════════════════════════════════
    #  FILE OPERATIONS
    # ══════════════════════════════════════════════

    def _new_file(self):
        if self.modified:
            if not messagebox.askyesno("WonkyPad", "Discard unsaved changes?"):
                return
        self.text.delete('1.0', 'end')
        self.current_file = None
        self.modified = False
        self.root.title("WonkyPad — Untitled")
        self.status_left.config(text="New file")
        self._schedule_update()

    def _open_file(self):
        filetypes = [
            ("All Supported", "*.py *.html *.htm *.css *.js *.txt *.md *.please *.mtg"),
            ("Python Files", "*.py"),
            ("HTML Files", "*.html *.htm"),
            ("All Files", "*.*"),
        ]
        path = filedialog.askopenfilename(filetypes=filetypes)
        if not path:
            return

        try:
            with open(path, 'r', encoding='utf-8') as f:
                content = f.read()
        except Exception as e:
            messagebox.showerror("WonkyPad", f"Error opening file:\n{e}")
            return

        self.text.delete('1.0', 'end')
        self.text.insert('1.0', content)
        self.current_file = path
        self.modified = False

        # Auto-detect mode
        ext = os.path.splitext(path)[1].lower()
        if ext in ('.html', '.htm', '.css'):
            self._set_mode('html')
        else:
            self._set_mode('python')

        self.root.title(f"WonkyPad — {os.path.basename(path)}")
        self.status_left.config(text=f"Opened {os.path.basename(path)}")
        self._schedule_update()

    def _save_file(self):
        if self.current_file:
            self._write_file(self.current_file)
        else:
            self._save_as()

    def _save_as(self):
        ext = '.py' if self.mode == 'python' else '.html'
        filetypes = [
            ("Python Files", "*.py"),
            ("HTML Files", "*.html"),
            ("All Files", "*.*"),
        ]
        path = filedialog.asksaveasfilename(
            defaultextension=ext,
            filetypes=filetypes
        )
        if not path:
            return
        self._write_file(path)

    def _write_file(self, path):
        try:
            content = self.text.get('1.0', 'end-1c')
            with open(path, 'w', encoding='utf-8') as f:
                f.write(content)
            self.current_file = path
            self.modified = False
            self.root.title(f"WonkyPad — {os.path.basename(path)}")
            self.status_left.config(text=f"Saved {os.path.basename(path)}")
        except Exception as e:
            messagebox.showerror("WonkyPad", f"Error saving file:\n{e}")

    def _quit(self):
        if self.modified:
            if not messagebox.askyesno("WonkyPad", "Discard unsaved changes?"):
                return
        self.root.destroy()

    # ══════════════════════════════════════════════
    #  UTILITY
    # ══════════════════════════════════════════════

    def _select_all(self):
        self.text.tag_add('sel', '1.0', 'end')
        return 'break'

    def _set_mode(self, mode):
        self.mode = mode
        label = "🐍 Python" if mode == 'python' else "🌐 HTML"
        self.mode_label.config(text=label)
        self.status_mode.config(text=mode.capitalize())
        self.status_left.config(text=f"Mode: {mode.capitalize()}")
        self._schedule_update()

    def _toggle_indent_guides(self):
        self.indent_guides_enabled = not self.indent_guides_enabled
        state = "ON" if self.indent_guides_enabled else "OFF"
        self.status_left.config(text=f"Indent guides: {state}")
        self._schedule_update()

    def _toggle_auto_close(self):
        self.auto_close_enabled = not self.auto_close_enabled
        state = "ON" if self.auto_close_enabled else "OFF"
        self.status_left.config(text=f"Auto-close: {state}")

    def _toggle_highlighting(self):
        self.highlight_enabled = not self.highlight_enabled
        state = "ON" if self.highlight_enabled else "OFF"
        self.status_left.config(text=f"Syntax highlighting: {state}")
        if not self.highlight_enabled:
            for tag in self.text.tag_names():
                if tag.startswith(('py_', 'html_')):
                    self.text.tag_remove(tag, '1.0', 'end')
        else:
            self._highlight_syntax()

    def _show_context_menu(self, event):
        try:
            self.context_menu.tk_popup(event.x_root, event.y_root)
        finally:
            self.context_menu.grab_release()

    def _show_about(self):
        messagebox.showinfo(
            "About WonkyPad",
            "WonkyPad v1.0\n\n"
            "A text editor that looks like Windows 95\n"
            "and respects the user.\n\n"
            "Features:\n"
            "• Ghost indent guides\n"
            "• Auto-closing brackets\n"
            "• Syntax highlighting\n"
            "• Wonky text transforms\n"
            "• Python & HTML modes\n\n"
            "DodecaGone Systems\n"
            "><^\n\n"
            "GNU TERRY PRATCHETT"
        )

    def _on_key_release(self, event):
        self.modified = True
        title = self.root.title()
        if not title.startswith("*"):
            self.root.title("*" + title)
        self._schedule_update()

    def _schedule_update(self):
        """Debounced update for all visual elements."""
        if hasattr(self, '_update_job'):
            self.root.after_cancel(self._update_job)
        self._update_job = self.root.after(100, self._do_update)

    def _do_update(self):
        self._highlight_syntax()
        self._update_indent_guides()
        self._update_line_numbers()
        self._update_status()

    def _update_status(self):
        try:
            pos = self.text.index('insert')
            line, col = pos.split('.')
            self.status_pos.config(text=f"Ln {line}, Col {int(col) + 1}")
        except:
            pass


def main():
    root = tk.Tk()

    # Try to set the icon on systems that support it
    try:
        root.iconname("WonkyPad")
    except:
        pass

    app = WonkyPad(root)
    root.mainloop()


if __name__ == '__main__':
    main()
