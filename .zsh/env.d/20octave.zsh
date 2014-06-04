# Octave supports "native" plotting using OpenGL and FLTK. You can activate
# it for all future figures using the Octave command

#     graphics_toolkit ("fltk")

# or for a specific figure handle h using

#     graphics_toolkit (h, "fltk")

# Otherwise, gnuplot is still used by default, if available.
# When plotting with gnuplot, you should set "GNUTERM=x11" before running octave;
# if you are using Aquaterm, use "GNUTERM=aqua".

export GNUTERM='x11'
