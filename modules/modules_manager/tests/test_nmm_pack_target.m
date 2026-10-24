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
% nmm('pack', ..., '-target', 'wasm32') cross-builds a binary module for the
% WebAssembly engine: the builder sees NELSON_DL_TARGET, the package is
% validated against the target platform, and its lockfile names wasm32.
% The fixture builder writes a placeholder gateway, so no Emscripten SDK is
% needed here; the real side-module build is covered by dlgeneratemake tests.
%=============================================================================
previousTarget = getenv('NELSON_DL_TARGET');
setenv('NELSON_DL_TARGET', '');
testRoot = [tempdir(), 'nmm_pack_target/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
moduleName = 'nmm_target_fixture';
moduleDir = [testRoot, moduleName, '/'];
mkdir(moduleDir);
evalc(['nmm(''init'', ''', moduleDir, ''', ''module'', ''', moduleName, ''', ''title'', ''T'', ''summary'', ''s'', ''license'', ''MIT'', ''Skeleton'', true);']);
manifest = jsondecode([moduleDir, 'module.json'], '-file');
manifest.builtin = true;
manifest.platforms = {'wasm32'};
filewrite([moduleDir, 'module.json'], jsonprettyprint(jsonencode(manifest)));
builder = {
  'moduleDir = fileparts(nfilename(''fullpath''));', ...
  'mkdir([moduleDir, ''/builtin'']);', ...
  'filewrite([moduleDir, ''/builtin/libnlsNmm_target_fixture_builtin'', getdynlibext()], getenv(''NELSON_DL_TARGET''));'};
filewrite([moduleDir, 'builder.m'], strjoin(builder, char(10)));
destination = [testRoot, 'out/'];
mkdir(destination);
%=============================================================================
% the module only supports wasm32: packing for the host is refused
asserts.checkerror('nmm(''pack'', moduleDir, destination)', sprintf('Platform not supported: %s', 'wasm32'));
%=============================================================================
% only the WebAssembly engine can be targeted from another host
asserts.checkerror('nmm(''pack'', moduleDir, destination, ''-target'', ''nope'')', 'Unsupported pack target: nope');
%=============================================================================
packageFile = nmm('pack', moduleDir, destination, '-target', 'wasm32');
asserts.istrue(endsWith(packageFile, [moduleName, '-wasm32-', manifest.version, '.nmz']));
asserts.istrue(isfile(packageFile));
asserts.isequal(getenv('NELSON_DL_TARGET'), '');
extractDir = [testRoot, 'extract/'];
unzip(packageFile, extractDir);
lock = jsondecode([extractDir, 'module-lock.json'], '-file');
asserts.isequal(lock.platform, 'wasm32');
asserts.isequal(lock.nelson.arch, 'wasm32');
asserts.isequal(lock.nelson.abi, 'wasm32-32');
% the builder ran with the target set and produced the side-module name
gateway = [extractDir, 'builtin/libnlsNmm_target_fixture_builtin.so'];
asserts.istrue(isfile(gateway));
asserts.isequal(fileread(gateway), 'wasm32');
%=============================================================================
% dry run reports the target archive name without creating it
report = nmm('pack', moduleDir, destination, '-dry-run', '-target', 'wasm32');
asserts.istrue(report.dry_run);
asserts.istrue(endsWith(report.package_file, [moduleName, '-wasm32-', manifest.version, '.nmz']));
asserts.isequal(report.lock.nelson.arch, 'wasm32');
%=============================================================================
setenv('NELSON_DL_TARGET', previousTarget);
rmdir(testRoot, 's');
%=============================================================================
