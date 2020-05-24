from setuptools import setup

with open("README.md",'r') as f:
    long_description = f.read()

with open('requirements.txt') as f:
    requirements_lines = f.readlines()
install_requires = [r.strip() for r in requirements_lines]

setup(
   name='Hasky',
   version='0.01dev',
   description='Import Haskell modules as if they were Python modules',
   license='LGPLv3',
   long_description=long_description,
   long_description_content_type='text/markdown',
   author='Simon Plakolb',
   author_email='s.plakolb@gmail.com',
   url="https://www.behaviour.space/",
   packages=['hasky','hasky.haskell'],
   package_dir={'hasky':'hasky','hasky.haskell':'hasky/haskell'},
   package_data={'hasky.haskell': ['res/*.c']},
   install_requires=install_requires,
)
