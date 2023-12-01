# Building Nelson

Depending on what platform or features you require, the build process may
differ slightly. After you've successfully built a binary, running the
test suite to validate that the binary works as intended is a good next step.

If you consistently can reproduce a test failure, search for it in the
[Nelson issue tracker](https://github.com/nelson-lang/nelson/issues) or
file a new issue.

## How to build Nelson ?

To compile Nelson, you will need a C++17 compiler.
Qt 5.15 or more required.

### [Linux](BUILDING_Linux.md)

### [Mac OS X](BUILDING_Macos.md)

### [Windows](BUILDING_Windows.md)

You can also see [ccpp.yml](https://github.com/nelson-lang/nelson/blob/master/.github/workflows/ccpp.yml) file to help you to see dependencies. This file is always the most up-to-date than this help.

## Code formatter

[prettier](https://prettier.io) is used to format `.xml`, `.json`, `.md` files.
Continuous integration checks that theses files are correctly formatted.
To fix format, it requires to have nodejs available on your pc.

```batch
npm install
npm run prettier:check
npm run prettier
```
