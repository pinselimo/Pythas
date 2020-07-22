from setuptools import setup

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
   package_data={'pythas.haskell': ['res/*.c','res/Foreign/*.hs'
                                  ,'res/cstructs-in-haskell/Foreign/C/*.hs'
                                  ,'res/cstructs-in-haskell/Foreign/C/Structs/*.hs'
                                  ,'ffi/*.hs','ffi/PythasFFI/*.hs'
                                  ,'bin/*.hi','bin/PythasFFI/*.hi'
                                  ,'bin/*.o','bin/PythasFFI/*.o']},
   install_requires=install_requires,
)
