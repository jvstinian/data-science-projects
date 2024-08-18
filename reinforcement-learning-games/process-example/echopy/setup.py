from setuptools import setup, find_packages

setup(
    name='echo',
    version='1.0.0',
    url='',
    author='Author Name',
    author_email='author@gmail.com',
    description='stdin echo',
    packages=find_packages(),    
    install_requires=[],
    entry_points={
        'console_scripts': [
            'echopy=echo.main:main',
        ],
    },

)
