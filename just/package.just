# Package the project.
# On macOS, CPack automatically runs the bundle fixup via CPACK_INSTALL_SCRIPT.
package:
    cmake --build . -- package

