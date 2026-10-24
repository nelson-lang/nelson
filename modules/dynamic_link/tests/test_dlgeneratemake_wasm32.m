%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% dlgeneratemake describes an Emscripten side module when NELSON_DL_TARGET is
% wasm32: the generated CMakeLists.txt includes the engine's shared settings
% instead of linking host libraries.
%=============================================================================
previousTarget = getenv('NELSON_DL_TARGET');
previousBuildDir = getenv('NELSON_WASM_BUILD_DIR');
restoreEnvironment = onCleanup(@() cellfun(@(n, v) setenv(n, v), ...
  {'NELSON_DL_TARGET', 'NELSON_WASM_BUILD_DIR'}, {previousTarget, previousBuildDir}));
%=============================================================================
root = [tempdir(), 'dlgeneratemake_wasm32/'];
if isdir(root)
  rmdir(root, 's');
end
mkdir([root, 'generated/wasm-runtime']);
mkdir([root, 'build']);
configFile = [root, 'generated/wasm-runtime/side-module.cmake'];
filewrite(configFile, 'set(NELSON_WASM_SIDE_MODULE_LINK_OPTIONS "-sSIDE_MODULE=2")');
sourceFile = [root, 'cpp_sum.cpp'];
filewrite(sourceFile, 'int cpp_sum(int a, int b) { return a + b; }');
setenv('NELSON_DL_TARGET', 'wasm32');
setenv('NELSON_WASM_BUILD_DIR', root);
%=============================================================================
[status, message] = dlgeneratemake([root, 'build'], 'libnlsDemo_builtin', {sourceFile}, {root});
asserts.istrue(status, message);
content = fileread([root, 'build/CMakeLists.txt']);
asserts.istrue(contains(content, ['include("', strrep(configFile, '\', '/'), '")']));
asserts.istrue(contains(content, 'NELSON_WASM_SIDE_MODULE_LINK_OPTIONS'));
asserts.istrue(contains(content, 'SUFFIX ".so"'));
asserts.isfalse(contains(content, '__WASM_SIDE_MODULE_CONFIG__'));
%=============================================================================
% executables have no wasm32 variant
asserts.checkerror('dlgeneratemake(''executable'', [root, ''build''], ''demo'', {sourceFile}, {root})', ...
  _('wasm32 target only builds dynamic libraries (side modules).'));
%=============================================================================
% a missing engine configuration is reported, not silently ignored
setenv('NELSON_WASM_BUILD_DIR', [root, 'missing']);
asserts.checkerror('dlgeneratemake([root, ''build''], ''libnlsDemo_builtin'', {sourceFile}, {root})', ...
  sprintf(_('WebAssembly engine configuration not found: %s (configure the wasm preset or set NELSON_WASM_BUILD_DIR).'), ...
  [root, 'missing/generated/wasm-runtime/side-module.cmake']));
%=============================================================================
setenv('NELSON_DL_TARGET', previousTarget);
setenv('NELSON_WASM_BUILD_DIR', previousBuildDir);
rmdir(root, 's');
%=============================================================================
