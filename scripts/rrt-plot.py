import sys
import matplotlib.pyplot as plt

edges = []
for line in open(sys.argv[1]):
    edges.append(tuple([float(token) for token in line.split()]))

for (x1,y1,x2,y2) in edges:
    plt.plot([x1, x2], [y1, y2], '-b')

plt.savefig(sys.argv[2] + '.png')
