"""
Tyrion
------

Tyrion is an awesome continuous deployment tool. That is all it does.
"""
from setuptools import Command, setup

setup(
    name='Tyrion',
    version='0.0.1',
    url='http://github.com/wjlroe/tyrion',
    license='BSD',
    author='William Roe',
    author_email='willroe@gmail.com',
    description='A tool for continuously deploying code from github',
    long_description=__doc__,
    packages=['tyrion'],
    platforms='any',
    install_requires=[
        'pika>=0.9.5',
        'Twisted>=11.1.0',
        'subprocess32>=3.2.0',
    ],
)
