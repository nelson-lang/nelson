%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--TIMEOUT 180-->
skip_testsuite(~ismodule('dynamic_link'), 'Native compiler module unavailable');
skip_testsuite(strcmp(dltarget(), 'wasm32'), 'A native executable target is required');
if ispc() && ~havecompiler()
  configuremsvc();
end
skip_testsuite(~havecompiler(), 'Native compiler unavailable');
skip_testsuite(~findcmake(), 'CMake unavailable');
%=============================================================================
destination = tempname();
mkdir(destination);
cleanupDirectory = onCleanup(@() rmdir(destination, 's')); %#ok<NASGU>
source = [modulepath('types'), '/tests/real_scalar_storage.cpp'];
[ok, message] = dlgeneratemake('executable', destination, ...
  'real_scalar_storage', source, dlgetnelsonincludes(), [], dlgetnelsonlibraries());
if ~ok
  error(message);
end
cmakeFile = [destination, '/CMakeLists.txt'];
cmake = fileread(cmakeFile);
cmake = replace(cmake, 'set(CMAKE_CXX_STANDARD 11)', 'set(CMAKE_CXX_STANDARD 17)');
cmake = replace(cmake, 'SET(CMAKE_SKIP_BUILD_RPATH TRUE)', 'SET(CMAKE_SKIP_BUILD_RPATH FALSE)');
filewrite(cmakeFile, cmake);
[ok, message] = dlmake(destination);
if ~ok
  error(message);
end
previousPath = getenv('PATH');
cleanupPath = onCleanup(@() setenv('PATH', previousPath)); %#ok<NASGU>
setenv('PATH', [modulepath('nelson', 'builtin'), pathsep(), previousPath]);
executable = [destination, '/real_scalar_storage'];
if ispc()
  executable = [executable, '.exe'];
end
[status, output] = unix(['"', executable, '"']);
if status ~= 0
  error('Nelson:types:scalarStorageTest', '%s', output);
end
asserts.istrue(contains(output, 'REAL_SCALAR_STORAGE_OK'));
%=============================================================================
