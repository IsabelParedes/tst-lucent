# Documentation

This documentation is created using the tool Sphinx.

It uses reStructuredText as its markup language, please refer to the [documentation](http://www.sphinx-doc.org/) for additional information.

## Sphinx

### Version

Sphinx 3.4.3

Python 3.8

### Installation

Installation using `pip`, [PyPi page](https://pypi.org/project/Sphinx/)

```bash
pip install sphinx
```

### Extensions

#### Read the docs theme (v0.5.1)

Installation using `pip`, [PyPi page](https://pypi.org/project/sphinx-rtd-theme/)

```bash
pip install sphinx-rtd-theme 
```

#### SphinxContrib Images (v0.9.2)

Installation using `pip`, [PyPi page](https://pypi.org/project/sphinxcontrib-images/)

```bash
pip install sphinxcontrib-images
```

### How to contribute

Modify or add files in the `source` directory.
Files added in the `source/modules` directory are automatically added to the table of content.
For now, any other file located elsewhere must be added to the `source/index.rst` file.

### How to build

From the root, open a prompt and type the following command to execute the make.bat script:

```bash
make html
```

It will update the files in the `build` directory.

### How to display documentation

Navigate to `build/html` and open the file `index.html` with your favorite browser.

## Documentation structure

One file per module in the `source/modules` directory.
