# Thanks to: https://i-systems.github.io/teaching/Optimization/iNotes/05_integer_prog.html
import sys
import re
import numpy as np
from scipy.optimize import milp, LinearConstraint, Bounds

def parse_line(line):
    m = re.search(r"\{([^}]*)\}", line)
    goals = np.fromstring(m.group(1), sep=",", dtype=float)
    button_strs = re.findall(r"\(([^)]*)\)", line)
    buttons = []
    for s in button_strs:
        s = s.strip()
        if not s:
            continue
        idxs = np.fromstring(s, sep=",", dtype=int)
        buttons.append(idxs)
    return goals, buttons

def search(goals, buttons):
    n = goals.shape[0]
    m = len(buttons)
    A = np.zeros((n, m), dtype=float)
    for i, idxs in enumerate(buttons):
        for j in idxs:
            A[j][i] += 1.0
    c = np.ones(m, dtype=float)
    constraints = LinearConstraint(A, goals, goals)
    integrality = np.ones(m, dtype=int)
    bounds = Bounds(lb=np.zeros(m), ub=np.full(m, np.inf))
    res = milp(
        c=c,
        constraints=[constraints],
        integrality=integrality,
        bounds=bounds,
    )

    return int(round(res.fun))

path = "input.txt"
total = 0
with open(path, "r", encoding="utf-8") as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        goals, buttons = parse_line(line)
        total += search(goals, buttons)

print(total)

