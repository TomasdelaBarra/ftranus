#!python
import sys
sys.path += ['..']
from pwant import * 
from pwant.tasks.delphi import dcc32

propdef('base_dir', '..')
import versioninfo

propdef('base_dir', '.')
propdef('obj_dir', '{base_dir}/obj')
propdef('bin_dir', '{base_dir}/bin')

@target
def clean():
    delfiles(prop('{obj_dir}/*'))
    delfiles(prop('{bin_dir}/*'))
    deldir(prop('obj_dir'))
    deldir(prop('bin_dir'))
    return call(['make', 'clean'])

@target
def versioninfo():
    filewrite('release.f90', prop(fileread('etc/release.py.f90'), False))
    filewrite('tranus.rc', prop(fileread('etc/tranus.py.rc'), False))

@target
def make():
    return call(['make'])

@target
def prepare():
    makedirs(prop('obj_dir'))
    makedirs(prop('bin_dir'))

@target
def all():
    pass

depends(prepare, [versioninfo])
depends(make, [prepare])
depends(all, [make])
depends(default, [all])

# these can be called from the command line
depends('clean', [clean])
depends('all',  [all])

if __name__ == '__main__':
    runbuild()
