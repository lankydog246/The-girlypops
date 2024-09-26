# Imports libraries
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.widgets import Slider, Button

# Creates a subplot and shifts it upwards to avoid the sliders
plt.subplots_adjust(bottom=0.35)

# Defines the the initial constants/variables
ε = 0
a = 1E-10
t = 1
N = int(1E6) # Sufficiently big N such that the cos wave looks continuous
k = np.pi/a

# Defines the initial x and y axes
x = np.linspace(-k*a, k*a, N)
y = ε - 2*t*np.cos(x)

# Adds a grid on the plot
plt.grid()

# Lables the graph
plt.suptitle("Band Structure")
plt.title(f"1st Brillouin Zone ({N=})")
plt.xlabel('k (1/a)')
plt.ylabel('E(k) (eV)')

# Stores the plot as a value
band_energy, = plt.plot(x, y)

# Defines the position and dimensions of the variables
ε_ax = plt.axes([0.15, 0.2, 0.65, 0.03])
a_ax = plt.axes([0.15, 0.15, 0.65, 0.03])
t_ax = plt.axes([0.15, 0.1, 0.65, 0.03])

# Defines the sliders for the variables
ε_slider = Slider(ε_ax, 'ε', -10, 10, ε)
a_slider = Slider(a_ax, 'a', 0.0, 1E-10, a)
t_slider = Slider(t_ax, 't', 0.0, 5, t)

# Defines a function that updates the plot if the
#   variables have been changed via the sliders
def update(val):
    ε = ε_slider.val
    a = a_slider.val
    t = t_slider.val
    k = np.pi/a
    x = np.linspace(-k*a, k*a, N)
    band_energy.set_ydata(ε - 2*t*np.cos(x))
    
# Calls the update function if the slider values get modified
ε_slider.on_changed(update)
a_slider.on_changed(update)
t_slider.on_changed(update)

# Shows the plot to the user
plt.show()
