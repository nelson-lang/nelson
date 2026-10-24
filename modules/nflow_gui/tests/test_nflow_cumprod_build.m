%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--TIMEOUT 180-->
% Builds the cumprod native block plugin example with the Nelson build tools
% (dlgeneratemake / dlmake) and the nflow SDK resolvers.
%=============================================================================
skip_testsuite(~ismodule('dynamic_link'), 'dynamic_link module unavailable');
skip_testsuite(strcmp(dltarget(), 'wasm32'), 'A native target is required');
if ispc() && ~havecompiler()
  configuremsvc();
end
skip_testsuite(~havecompiler(), 'Native compiler unavailable');
skip_testsuite(~findcmake(), 'CMake unavailable');
%=============================================================================
exampleDir = [modulepath('nflow_gui'), '/examples/libraries/cumprod'];
skip_testsuite(~isfile([exampleDir, '/cumprod.cpp']), 'Example source not installed');
%=============================================================================
source = [exampleDir, '/cumprod.cpp'];
buildDir = [tempname(), '_cumprod'];
mkdir(buildDir);
cleaner = onCleanup(@() rmdir(buildDir, 's')); %#ok<NASGU>
%=============================================================================
[ok, msg] = dlgeneratemake('dynamic_library', buildDir, 'nflow_cumprod', ...
  {source}, dlgetnflowincludes(), {}, dlgetnflowlibraries());
if ~ok
  error(msg);
end
[ok, msg] = dlmake(buildDir);
if ~ok
  error(msg);
end
%=============================================================================
built = [buildDir, '/nflow_cumprod', getdynlibext()];
asserts.istrue(isfile(built));
%=============================================================================
