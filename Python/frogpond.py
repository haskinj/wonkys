#!/usr/bin/env python3
"""
FROGPOND v1.0 — A language that only compiles in haiku.
'><^' GNU TERRY PRATCHETT | DodecaGone Systems 2026
"""
import sys, re, os

SYLLABLE_OVERRIDES = {
    "pond":1,"frog":1,"jump":1,"jumps":1,"splash":1,"drip":1,"drips":1,
    "ripple":2,"stone":1,"stones":1,"moon":1,"reed":1,"reeds":1,
    "flow":1,"flows":1,"still":1,"catch":1,"release":2,"croak":1,"croaks":1,
    "lily":2,"lotus":2,"reflect":2,"reflects":2,"sink":1,"rise":1,"rises":2,
    "deep":1,"shallow":2,"surface":2,"beneath":2,"silence":2,"listen":2,
    "gather":2,"scatter":2,"true":1,"false":1,"equal":2,"equals":2,
    "above":2,"below":2,"times":1,"once":1,"twice":1,"print":1,"say":1,
    "speak":1,"tell":1,"ask":1,"answer":2,"the":1,"a":1,"an":1,"is":1,
    "are":1,"and":1,"or":1,"not":1,"if":1,"then":1,"while":1,"for":1,
    "in":1,"of":1,"to":1,"it":1,"its":1,"with":1,"from":1,"by":1,
    "at":1,"on":1,"as":1,"be":1,"but":1,"no":1,"yes":1,"so":1,"do":1,
    "my":1,"your":1,"our":1,"their":1,"this":1,"that":1,"each":1,"all":1,
    "here":1,"there":1,"where":1,"when":1,"now":1,"one":1,"two":1,
    "three":1,"four":1,"five":1,"six":1,"seven":2,"eight":1,"nine":1,
    "ten":1,"zero":2,"hundred":2,"thousand":2,"water":2,"river":2,
    "stream":1,"rain":1,"snow":1,"wind":1,"sky":1,"cloud":1,"earth":1,
    "fire":2,"light":1,"dark":1,"night":1,"day":1,"dawn":1,"dusk":1,
    "spring":1,"summer":2,"autumn":2,"winter":2,"flower":2,"tree":1,
    "leaf":1,"leaves":1,"bird":1,"fish":1,"crane":1,"heron":2,
    "empty":2,"full":1,"begin":2,"end":1,"start":1,"stop":1,"open":2,
    "close":1,"hold":1,"holds":1,"give":1,"add":1,"minus":2,"plus":1,
    "more":1,"less":1,"than":1,"nothing":2,"something":2,"value":2,
    "number":2,"count":1,"many":2,"few":1,"some":1,"into":2,"onto":2,
    "over":2,"under":2,"through":1,"across":2,"between":2,"becomes":2,
    "become":2,"resting":2,"waiting":2,"floating":2,"falling":2,
    "rising":2,"growing":2,"gently":2,"softly":2,"slowly":2,
    "quietly":3,"forever":3,"moment":2,"remember":3,"forget":2,
    "morning":2,"evening":2,"clearly":2,"after":2,"before":2,
    "again":2,"away":2,"down":1,"up":1,"old":1,"new":1,"clear":1,
    "world":1,"hello":2,"peers":1,"deeply":2,"done":1,"counting":2,
    "begun":2,"begins":2,"gone":1,"friend":1,"what":1,"waits":1,
    "fills":1,"has":1,"just":1,"very":2,"ever":2,"sits":1,"last":1,
    "words":1,"result":2,"total":2,"ripples":2,"sure":1,"only":2,
}

def count_syllables(word):
    clean = re.sub(r'[^a-z]', '', word.lower().strip())
    if not clean: return 0
    if clean in SYLLABLE_OVERRIDES: return SYLLABLE_OVERRIDES[clean]
    vowels = "aeiouy"; count = 0; prev = False
    for c in clean:
        v = c in vowels
        if v and not prev: count += 1
        prev = v
    if clean.endswith("e") and count > 1 and not clean.endswith("le"): count -= 1
    return max(1, count)

def count_line_syllables(line):
    if "//" in line: line = line[:line.index("//")]
    return sum(count_syllables(w) for w in re.findall(r"[a-zA-Z']+", line))

def parse_stanzas(source):
    lines = source.split("\n"); stanzas = []; cl = []; cn = []
    for i, line in enumerate(lines, 1):
        s = line.strip()
        if s.startswith("#"): continue
        if not s:
            if cl: stanzas.append({"lines":cl,"line_numbers":cn}); cl=[]; cn=[]
            continue
        cl.append(s); cn.append(i)
    if cl: stanzas.append({"lines":cl,"line_numbers":cn})
    return stanzas

def validate_haiku(stanzas):
    errors = []; valid = []
    for sn, st in enumerate(stanzas, 1):
        ls, ns = st["lines"], st["line_numbers"]
        if len(ls) != 3:
            errors.append(f"Stanza {sn} (line {ns[0]}): must be 3 lines, got {len(ls)}"); continue
        exp = [5,7,5]; ok = True
        for i,(l,n) in enumerate(zip(ls,ns)):
            s = count_line_syllables(l)
            if s != exp[i]: errors.append(f'Line {n}: "{l}" has {s} syllables, expected {exp[i]}'); ok=False
        if ok: valid.append({"lines":ls,"line_numbers":ns,"stanza_num":sn})
    return valid, errors

class FrogPondInterpreter:
    def __init__(self, debug=False):
        self.vars = {}; self.output = []; self.debug = debug; self.running = True

    def resolve(self, token):
        t = token.strip().lower()
        if t.startswith('"') and t.endswith('"'): return t[1:-1]
        try: return int(t) if "." not in t else float(t)
        except: pass
        if t in self.vars: return self.vars[t]
        nums = {"zero":0,"one":1,"two":2,"three":3,"four":4,"five":5,
                "six":6,"seven":7,"eight":8,"nine":9,"ten":10,"nothing":0,"none":0}
        if t in nums: return nums[t]
        return t

    def eval_expr(self, text):
        t = text.strip().lower()
        for op, fn in [(" plus ",lambda a,b:a+b),(" minus ",lambda a,b:a-b),
                       (" times ",lambda a,b:a*b),(" over ",lambda a,b:a/b if b else 0)]:
            if op in t:
                p = t.split(op, 1)
                try: return fn(float(self.resolve(p[0].strip())),float(self.resolve(p[1].strip())))
                except: return 0
        return self.resolve(t)

    def execute_line(self, line):
        if not self.running: return
        low = line.strip().lower()
        if self.debug: print(f"  [exec] {line}")

        # SILENCE — halt
        if "silence" in low and ("falls" in low or "here" in low):
            self.running = False; return

        # QUOTED STRING OUTPUT — say/speak "text"
        quote = re.search(r'"([^"]*)"', line)
        if quote and re.match(r'(?:speak|say|sing|tell|croak)\s', low):
            self.output.append(quote.group(1)); return

        # CROAK variable — print a single variable's value
        m = re.match(r'(?:croak|tell|say|speak)\s+(?:the\s+)?(\w+)', low)
        if m:
            varname = m.group(1)
            # Skip filler words
            if varname not in ("ever","so","now","here","there","softly","gently",
                               "slowly","quietly","clearly","deeply","sure","words","word"):
                self.output.append(str(self.resolve(varname))); return

        # POND assignment — pond X holds Y
        m = re.match(r'pond\s+(\w+)\s+holds?\s+(.+)', low)
        if m:
            name = m.group(1)
            # Strip trailing filler words
            val_text = re.sub(r'\s+(now|here|there|gently|softly|slowly|quietly|clearly|deeply|sure|my|friend)(\s|$)', ' ', m.group(2)).strip()
            self.vars[name] = self.eval_expr(val_text)
            if self.debug: print(f"  [{name} = {self.vars[name]}]")
            return

        # X holds Y (non-pond)
        m = re.match(r'(?:the\s+)?(\w+)\s+holds?\s+(.+)', low)
        if m and m.group(1) not in ("pond","frog","if","while","silence","water","old","surface","morning"):
            name = m.group(1)
            val_text = re.sub(r'\s+(now|here|there|gently|softly|slowly|quietly|clearly|deeply|sure|my|friend)(\s|$)', ' ', m.group(2)).strip()
            self.vars[name] = self.eval_expr(val_text)
            if self.debug: print(f"  [{name} = {self.vars[name]}]")
            return

        # X becomes Y
        m = re.match(r'(\w+)\s+becomes?\s+(.+)', low)
        if m:
            name = m.group(1)
            val_text = re.sub(r'\s+(now|here|there|gently|softly|slowly|quietly|clearly|deeply|sure)(\s|$)', ' ', m.group(2)).strip()
            self.vars[name] = self.eval_expr(val_text)
            if self.debug: print(f"  [{name} = {self.vars[name]}]")
            return

        # ADD X to Y
        m = re.match(r'add\s+(.+?)\s+to\s+(?:the\s+)?(\w+)', low)
        if m:
            val = self.resolve(m.group(1).strip())
            name = m.group(2)
            if name in self.vars:
                try: self.vars[name] = float(self.vars[name]) + float(val)
                except: pass
                if self.debug: print(f"  [{name} = {self.vars[name]}]")
            return

        # TAKE X from Y
        m = re.match(r'take\s+(.+?)\s+from\s+(?:the\s+)?(\w+)', low)
        if m:
            val = self.resolve(m.group(1).strip())
            name = m.group(2)
            if name in self.vars:
                try: self.vars[name] = float(self.vars[name]) - float(val)
                except: pass
            return

        # ASK / LISTEN
        m = re.match(r'(?:ask|listen\s+to)\s+(?:the\s+)?(\w+)', low)
        if m:
            try:
                val = input(f"  [{m.group(1)}?] > ")
                try: val = float(val) if "." in val else int(val)
                except: pass
                self.vars[m.group(1)] = val
            except EOFError: self.vars[m.group(1)] = 0
            return

        # No-op (poetry)
        if self.debug: print(f"  [poetry] {line}")

    def check_cond(self, line):
        low = line.lower().strip()
        for pat, op in [
            (r'(?:if|when|while)\s+(\w+)\s+(?:is\s+)?above\s+(\w+)', lambda a,b:a>b),
            (r'(?:if|when|while)\s+(\w+)\s+(?:is\s+)?below\s+(\w+)', lambda a,b:a<b),
            (r'(?:if|when|while)\s+(\w+)\s+(?:is\s+)?equals?\s+(\w+)', lambda a,b:a==b),
        ]:
            m = re.match(pat, low)
            if m:
                try: return op(float(self.resolve(m.group(1))),float(self.resolve(m.group(2))))
                except: return False
        m = re.match(r'(?:if|when|while)\s+(\w+)\s+is\s+still', low)
        if m:
            try: return float(self.resolve(m.group(1))) != 0
            except: return bool(self.resolve(m.group(1)))
        m = re.match(r'(?:if|when|while)\s+(\w+)\s+is\s+empty', low)
        if m:
            try: return float(self.resolve(m.group(1))) == 0
            except: return not bool(self.resolve(m.group(1)))
        return False

    def execute_stanza(self, stanza):
        if not self.running: return
        lines = stanza["lines"]; low0 = lines[0].lower().strip()
        if low0.startswith(("if ","when ")):
            self.execute_line(lines[1] if self.check_cond(lines[0]) else lines[2]); return
        if low0.startswith("while "):
            for _ in range(10000):
                if not self.check_cond(lines[0]) or not self.running: break
                self.execute_line(lines[1]); self.execute_line(lines[2])
            return
        for l in lines: self.execute_line(l)

    def run(self, source):
        stanzas = parse_stanzas(source)
        valid, errors = validate_haiku(stanzas)
        if errors:
            print("\n  ~ The pond rejects your offering ~\n")
            for e in errors: print(f"  {e}")
            print("\n  Syllable breakdown:")
            for st in stanzas:
                for l,n in zip(st["lines"],st["line_numbers"]):
                    ws = re.findall(r"[a-zA-Z']+", l)
                    print(f'    Line {n}: [{count_line_syllables(l)}] {" + ".join(f"{w}({count_syllables(w)})" for w in ws)}')
                print()
            return False
        if not valid: print("\n  ~ The pond is empty ~\n"); return False
        for s in valid:
            self.execute_stanza(s)
            if not self.running: break
        for l in self.output: print(l)
        if self.debug:
            print(f"\n  ~ Silence ~")
            if self.vars: print(f"  Vars: {self.vars}")
        return True

EXAMPLES = {
"hello": """# Hello World in FrogPond

say "hello world" here
the old pond waits here and now
a frog jumps in splash""",

"count": """# Counting to five

pond count holds zero
the surface is still and clear
the morning begins

while count below five
croak count ever so softly
add one to the count

say "done" counting now
the ripples have all gone still
silence falls down here""",

"math": """# Arithmetic

pond sun holds seven
the surface holds the moon three
the sky holds nothing

sky holds sun plus moon
the sky becomes the result
croak the sky softly

sky holds sun times moon
the product ripples softly
croak sky into night""",

"conditional": """# Conditional logic

pond depth holds eight now
the water is deep and dark
the frog peers below

if depth above five
say "the old pond is deep" now
say "shallow" water

say "and that is that"
the frog sits on the stone now
silence falls at last""",
}

def show_examples(name=None):
    names = [name] if name and name in EXAMPLES else list(EXAMPLES.keys())
    for n in names:
        print(f"\n{'='*50}\n  EXAMPLE: {n}\n{'='*50}\n")
        for l in EXAMPLES[n].split("\n"): print(f"  {l}")
        print(f"\n  --- Output ---")
        FrogPondInterpreter().run(EXAMPLES[n])
        print()

def repl():
    print("\n  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print("  ~  FROGPOND v1.0 — Interactive Pond    ~")
    print("  ~  Enter 3-line haiku stanzas           ~")
    print("  ~  Blank line to execute                ~")
    print("  ~  'quit' to leave                      ~")
    print("  ~  '><^' GNU TERRY PRATCHETT            ~")
    print("  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    interp = FrogPondInterpreter(debug=True); buf = []
    while True:
        try:
            line = input(f"  {'~' if buf else '·'} ")
            if line.strip().lower() in ("quit","exit","bye"):
                print("\n  ~ The frog departs ~\n"); break
            if not line.strip() and buf:
                src = "\n".join(buf); st = parse_stanzas(src); v,e = validate_haiku(st)
                if e:
                    print("\n  ~ The pond rejects your offering ~")
                    for err in e: print(f"  {err}")
                    for s in st:
                        for l,n in zip(s["lines"],s["line_numbers"]):
                            ws = re.findall(r"[a-zA-Z']+", l)
                            print(f'    [{count_line_syllables(l)}] {" + ".join(f"{w}({count_syllables(w)})" for w in ws)}')
                    print()
                else:
                    for s in v:
                        interp.execute_stanza(s)
                        for o in interp.output: print(f"  >> {o}")
                        interp.output.clear()
                buf = []
            elif line.strip(): buf.append(line.strip())
        except (EOFError, KeyboardInterrupt): print("\n\n  ~ The frog departs ~\n"); break

def main():
    if len(sys.argv) < 2:
        print("\n  FROGPOND v1.0 — Code that must be poetry.\n")
        print("    python3 frogpond.py <file.fp>       Run a program")
        print("    python3 frogpond.py --example        Show examples")
        print("    python3 frogpond.py --repl           Interactive mode")
        print("    python3 frogpond.py --check \"line\"   Check syllables")
        print("\n  '><^' GNU TERRY PRATCHETT\n"); return
    arg = sys.argv[1]
    if arg == "--example": show_examples(sys.argv[2] if len(sys.argv)>2 else None)
    elif arg == "--repl": repl()
    elif arg == "--check" and len(sys.argv)>2:
        t = " ".join(sys.argv[2:]); ws = re.findall(r"[a-zA-Z']+",t)
        print(f'  [{count_line_syllables(t)}] {" + ".join(f"{w}({count_syllables(w)})" for w in ws)}')
    elif os.path.exists(arg):
        with open(arg) as f: FrogPondInterpreter().run(f.read())
    else: print(f"  File not found: {arg}")

if __name__ == "__main__": main()
