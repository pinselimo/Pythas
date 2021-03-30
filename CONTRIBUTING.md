# Contributing to Pythas

Thanks for using *Pythas* und considering contributing to it! Contributions to *Pythas* are very welcome! Raise an issue if you found a bug, have a feature request or any other interesting ideas!

## Usage examples

*Pythas*' use case scenarios are vast and yet examples are limited. If you think you have an interesting application of *Pythas* head over to the [Pythas Examples repo](https://github.com/pinselimo/Pythas-Examples) and show it to the world!

## Bugs and feature requests

*Pythas* has been split into multiple packages by now. If you encountered a bug using *Pythas* please leave an issue in the [main repository](https://github.com/pinselimo/Pythas). In case the issue affects another package the authors will link the initial issue there.
However, if instead you found a bug in a specific sub-package (e.g. Pythas-FFI), feel free to leave the issue there.

Feature requests that affect the UX should always be posted on the main repo. Only improvements to the sub-packages that won't affect the interfacing and/or UX should be posted directly in the sub-package repository.

### Contribution guidelines

The following guidelines should be followed when Contributing to *Pythas*:

+ Please raise an issue first to establish a discussion about your inquiry.
+ After the issue has been deemed worthwile, feel free to post a pull request.
+ Please ensure the testing suite executes successfully before starting a pull request.
+ Respond to the code review and re-test in case changes are required.

### Testing

Running the test suite is as easy as:

~~~bash
$ pip install -r requirements-dev.txt
$ pytest
$ black --check .
~~~

