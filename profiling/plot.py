import sys
import pandas as pd
from plotnine import *

if len(sys.argv) < 2:
    print("Usage:", sys.argv[0], " <filename>")
    sys.exit(1)

filename = sys.argv[1]
df = pd.read_csv(filename)
df['N'] = df['N'].astype(int)
order = ['Fortran', 'Sequential', 'Threaded', 'Distributed 2 Nodes', 'Distributed 4 Nodes']
df['Version'] = pd.Categorical(df['Version'], categories=order, ordered=True)

df['Time Per Initialization (s)'] = df['Time (s)'] / df['N']

p = ggplot(df, aes(x='N', y='Time (s)', color='Version')) + geom_point() + geom_line() + ggtitle("MARBL Geometry Initialization") + theme(
        axis_text=element_text(size=14),      # Axis tick labels
        axis_title=element_text(size=16),     # Axis titles
        legend_text=element_text(size=14),    # Legend text
        legend_title=element_text(size=16)    # Legend title
    ) + scale_y_continuous(limits=(0, max(df['Time (s)']))) + scale_x_continuous(limits=(0, max(df['N']))) + scale_color_manual(values=['red', 'blue', 'green', 'purple', 'orange'])
p.save('initialization-times.png')

p = ggplot(df, aes(x='N', y='Time Per Initialization (s)', color='Version')) + geom_point() + geom_line() + ggtitle("MARBL Geometry Initialization") + theme(
        axis_text=element_text(size=14),      # Axis tick labels
        axis_title=element_text(size=16),     # Axis titles
        legend_text=element_text(size=14),    # Legend text
        legend_title=element_text(size=16)    # Legend title
    ) + scale_y_continuous(limits=(0, max(df['Time Per Initialization (s)']))) + scale_x_continuous(limits=(0, max(df['N']))) + scale_color_manual(values=['red', 'blue', 'green', 'purple', 'orange'])
p.save('per-initialization-times.png')