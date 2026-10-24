%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A module whose startup fails on this host (for example a gateway built for
% another architecture by nmm('pack', ..., '-target')) must not abort the
% build: nmm_build_help reports it and leaves the help sources to the
% installing engine.
%=============================================================================
moduleName = 'nmm_unloadable_fixture';
moduleDir = [tempdir(), moduleName, '/'];
if isdir(moduleDir)
  rmdir(moduleDir, 's');
end
mkdir([moduleDir, 'etc']);
mkdir([moduleDir, 'help/en_US/xml']);
filewrite([moduleDir, 'etc/startup.m'], 'error(''gateway built for another architecture'');');
filewrite([moduleDir, 'etc/finish.m'], '');
filewrite([moduleDir, 'loader.m'], ['addmodule(fileparts(nfilename(''fullpath'')), ''', moduleName, ''');']);
%=============================================================================
output = evalc('nmm_build_help(moduleName, moduleDir);');
asserts.istrue(contains(output, 'help not generated'));
asserts.istrue(contains(output, 'gateway built for another architecture'));
asserts.isfalse(ismodule(moduleName));
asserts.isfalse(isdir([moduleDir, 'help/en_US/html']));
%=============================================================================
% a cross build never tries to load the module on the host
previousTarget = getenv('NELSON_DL_TARGET');
setenv('NELSON_DL_TARGET', 'wasm32');
filewrite([moduleDir, 'etc/startup.m'], 'error(''must not run'');');
output = evalc('nmm_build_help(moduleName, moduleDir);');
setenv('NELSON_DL_TARGET', previousTarget);
asserts.istrue(contains(output, 'wasm32 target'));
asserts.isfalse(contains(output, 'must not run'));
asserts.isfalse(ismodule(moduleName));
%=============================================================================
rmdir(moduleDir, 's');
%=============================================================================
