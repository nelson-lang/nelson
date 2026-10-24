# Trademark Notice

Nelson is an independent, community-developed free software project. It is **not
affiliated with, endorsed by, or sponsored by** any of the companies or projects
whose trademarks are acknowledged below.

## Acknowledgements

- MATLAB and Simulink are registered trademarks of The MathWorks, Inc.
- GNU Octave is a project of the Free Software Foundation.
- Scilab and Xcos are trademarks of their respective owners.
- All other product names, logos and brands mentioned in this repository are
  the property of their respective owners.

Use of these names in this repository is **nominative**: they identify the
products they designate, for the purposes of interoperability, comparison, or
attribution. Such use does not imply any affiliation or endorsement.

## Where these names appear, and why

Nelson's coding policy forbids brand names in new source code
and documentation. The following residual occurrences are deliberate and
necessary:

| Location | Nature of the reference |
| --- | --- |
| `modules/mex/` | API identifiers (`mexCallMATLAB`, `mexCallMATLABWithTrap`, ...) required for **source compatibility** of third-party MEX files. Renaming them would defeat the purpose of the module. |
| `modules/matio/`, `modules/audio/` | Identifiers of documented **file formats** (MAT-file version strings, WAVE metadata), needed to read and write those files. |
| `modules/help_tools/`, `` ```matlab `` fenced blocks | **Syntax-highlighting language identifiers** consumed by the third-party highlight.js library. These are technical identifiers, not descriptions of the product. |
| `modules/slicot/` | A **bibliographic citation** present in the upstream SLICOT reference sources (BSD-3-Clause), retained as required by that license. |
| `desktop/` | The freedesktop **MIME type** `text/x-matlab`, and an AppStream description comparing Nelson's syntax to existing environments so users can judge portability. Trademarks are marked with `®` there. |
| `CHANGELOG*.md` | **Historical release notes**, kept unmodified as a record of the project's history. |

## Independence of the implementation

Nelson is an original implementation. Its source code, documentation and test
suites are written by the Nelson contributors and distributed under the terms
stated in `lgpl-3.0.md` and `gpl-3.0.md`. No proprietary source code,
documentation or other copyrighted material from any third-party numerical
computing product has been copied into this repository.

Implementing a compatible language, function names, or file formats is an
interoperability activity: functional interfaces, names and behaviors are not
themselves protected by copyright.

Third-party components redistributed with Nelson, together with their own
licenses, are listed in `THIRDPARTY.md`.
