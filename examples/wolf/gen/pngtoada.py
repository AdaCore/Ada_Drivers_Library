#!/usr/bin/env python

import png
import os

def gen_ada(filename, pkg, dest):
    with open(dest, 'w') as fout:
        fout.write("package %s is\n\n" % pkg);

        with open(filename, "rb") as finput:
            r = png.Reader(file=finput)
            (width, height, pixels, properties) = r.asRGBA()
            fout.write("   Bmp : aliased constant Texture :=\n")
            n_line = 1
            for line in pixels:
                cols = []
                for j in range(width):
                    a = line[4 * j + 3] >> 7
                    if a == 0:
                        r = 0
                        g = 0
                        b = 0
                    else:
                        r = line[4 * j] >> 3
                        g = line[4 * j + 1] >> 2
                        b = line[4 * j + 2] >> 3
                    val = (r << 11) | (g << 5) | b
                    cols.append ('16#%x#' % val)
                if n_line == 1:
                    fout.write("      (")
                else:
                    fout.write("       ")
                fout.write("(%s)" %  ", ".join(cols))
                if n_line == height:
                    fout.write(");\n")
                else:
                    fout.write(",\n")
                n_line += 1
            fout.write('   pragma Linker_Section (Bmp, ".ccmdata");\n')
        fout.write("\nend %s;\n" % pkg)

def is_png(pics, fname):
    base, ext = os.path.splitext(fname)
    return os.path.isfile(os.path.join(pics, fname)) and ext == '.png'

pics = os.path.abspath("./pics")
files = [f for f in os.listdir(pics) if is_png(pics, f)]
for f in files:
    pkg, _ = os.path.splitext(os.path.basename(f))
    src = os.path.join('..', 'pics', 'textures-%s.ads' % pkg)
    pkg = "Textures.%s" % pkg[0].upper() + pkg[1:]
    print src
    gen_ada(os.path.join(pics, f), pkg, src)
