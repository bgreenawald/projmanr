# Contributing for projmanr

Thank you so much for your interest in contributing to projmanr. I am currently the only person working on this project so any help is greatly appreciated!

## What are the different ways I can contribute?

There are a number of ways to contribute to this project!

1. Testing contributions. This is by far the easiest way to contribute. All you have to do is use the package and if something breaks, let me know! The preferred way to track this is to open an issue in Github, but if you aren't comfortable doing this, you can also email me at bgreenawald@gmail.com and I can open the issue myself.
2. Idea contributions. This is another really easy way to contribute. As I discuss in the blog post for this project, I don't actually have a background in project management. I initially built this package for a business professor and she was guiding the feature creation for this project. So if you are in project management and have ideas for how this package could be useful, please let me know. Again, the simplest way is to open an issue, but email also works.
3. Code contributions. If you're ready to dive in and actually contribute to the code base of this project, fantastic! If you want to help develop planned features, check to see what's available in the issues tab. If you have some ideas you want to take a swing at, open an issue so that I know what's being worked on. If you want your code to eventually be a part of the codebase, keep reading below for the workflow and coding standards.

## Coding contribution

Anyone wishing to contribute to this package should first read Hadley Wickham's book on R package development. The book is a fairly short read and can be found for free [here](http://r-pkgs.had.co.nz/). This package was developed almost entirely following the instruction of that book, so reading it will give you most of what you need to contribute.

## Code development workflow

This project follows the standard fork and pull request workflow. You can read about that [here](https://gist.github.com/Chaser324/ce0505fbed06b947d962).

## Code organization

Any 'public' functions (i.e functions that a user can call using the package), should exist in the 'critical_path.R' file. These should be appended to what already exists.

Any helper functions should go in the 'utility_functions.R' file, again, being appended to what already exists.

## Code style

The coding style of this package is enforced by the 'lintr' package. After installing it, run

```R
lintr::lint_package()
```

and correct all style errors identified.

## Code commenting

All 'public' functions are required to have roxygen2 commenting as defined [here](http://r-pkgs.had.co.nz/man.html). These functions should at the very least have a function description, a description of all parameters, and a description of the return value. Any additional commenting fields (i.e usage, examples) are recommended but not required.

There should also be liberal use of commenting throughout the code, with a line comment for every line or two of non-trivial code being the preferred frequency.

Currently, all non-public functions have only a line of two of line comments for docstrings, however I am currently in the process of transitioning to use the standards defined in the 'docstring' package, so all new functions should follow this guide.

## Code testing

Rather than unit testing, this package testing structure has largely been built around functional testing. In other words, toy data sets are set up and run through a variety of tests to ensure the functions are returning desired results. At some point in the future, I do intend to add unit tests, but for now, functional tests will suffice.

Any modifications or additions to the code should not break any of the current tests, and additional tests should be added to the code for any change or addition to functionality. For this, it's easiest to look at the tests that have already been written and follow those as an example.

Further, all code must pass all of CRAN and Rstudio's package checks for R. In Rstudio, you can test this by running the 'Build' -> 'Check' command, or 'Ctrl+Shift+E'. You should also add the following flags to your Build Tool Configuration for checks: '--no-manual --as-cran'

## C++ development

NEED TO ADD.

## Submitting pull requests

When submitting pull requests, I ask that you include a description of the features you have included, the tests that you have run on that feature, and the results of running a check on the package. Further, please include information about the operating system that you have developed and tested the code on.

## Additional questions

If you have any questions about development, please email them to me directly instead of opening them up as issues. Thanks in advance for your contributions!
