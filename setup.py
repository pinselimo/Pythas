from setuptools import setup

cstructs = ['res/cstructs-in-haskell/src/Foreign/C/'+s for s in ['*.hs','Structs/*.hs']]
pythastypes = ['res/Pythas-Types/src/Foreign/Pythas/*.hs']
pythasffi = ['ffi/src/*.hs'] + ['ffi/src/Foreign/'+s for s in ['*.hs','Pythas/*.hs']]
outputs = ['bin/'] + ['bin/Foreign/'+s for s in ['','C/','Pythas/','C/Structs/']]
os = [s+'*.o' for s in outputs]
his = [s+'*.hi' for s in outputs]

with open("README.md",'r') as f:
    long_description = f.read()

with open('requirements.txt') as f:
    requirements_lines = f.readlines()
install_requires = [r.strip() for r in requirements_lines]

setup(
   name='Pythas',
   version='0.01dev',
   description='Import Haskell modules as if they were Python modules',
   license='LGPLv3',
   long_description=long_description,
   long_description_content_type='text/markdown',
   author='Simon Plakolb',
   author_email='s.plakolb@gmail.com',
   url="https://www.behaviour.space/",
   packages=['pythas','pythas.haskell'],
   package_dir={'pythas':'pythas','pythas.haskell':'pythas/haskell'},
   package_data={'pythas.haskell': ['res/*.c']+cstructs+pythastypes+pythasffi+os+his},
   install_requires=install_requires,
)

