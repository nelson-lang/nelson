# Package the project.
# On macOS, CPack automatically runs the bundle fixup via CPACK_PRE_BUILD_SCRIPTS.
package:
    cmake --build . -- package

