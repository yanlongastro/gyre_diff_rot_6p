"""
Install package stellarmodels.

Example usage: without pip:

$:> python setup.py build
$:> sudo python setup.py install

With pip:

$:> python setup.py sdist
$:> sudo pip install dist/stellarmodels-0.0.tar.gz

And with pip you can uninstall:

$:> sudo pip uninstall stellarmodels

On *buntu systems, the installation directory is

/usr/local/lib/python2.7/dist-packages/stellarmodels

"""
from numpy.distutils.core import setup, Extension
import glob
import sys
from numpy.distutils.command.build import build as _build    
        
setup(
    
    name="gyre",
    version="0.0",
    description="Read in GYRE files",
    long_description="reStructuredText format",
    author="Pieter Degroote",
    author_email="pieterd@ster.kuleuven.be",

    packages=['gyre'],
    
    entry_points = {
        "console_scripts": ["gyre2ascii = gyre.gyre:main"]}

)


