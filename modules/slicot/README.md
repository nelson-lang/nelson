## License

Note that SLICOT is licensed under GPLv2 or higher, but the bindings
to the library in this nelson's module, is licensed under LGPL2 v2.1.
This means that code using the SLICOT library via SLICOT bindings is subject to SLICOT's licensing terms.

If you distribute a derived or combined work, i.e. a program that links to and is distributed with the SLICOT library, then that distribution falls under the terms of the GPL.

if SLICOT library is available on platform, and user chooses to use it, distribution falls under the terms of the GPL.

On Windows and others platforms use of SLICOT is optional:

On linux and macos, you can build Nelson without SLICOT wrapper

```bash
cmake -DLGPL_V21_ONLY=ON .
```

On Windows, on setup, user can choose to install libslicot.dll or not.
