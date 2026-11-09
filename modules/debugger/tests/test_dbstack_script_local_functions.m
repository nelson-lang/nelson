%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
filename = [path_test, '/script_local_debug_stack.m'];
filewrite(filename, ["debugStackResult = scriptLocalDebugHelper();"; ...
  "secondStackResult = secondLocalDebugHelper();"; ...
  "function st = scriptLocalDebugHelper()"; ...
  "  st = dbstack();"; ...
  "end"; ...
  "function st = secondLocalDebugHelper()"; ...
  "  st = dbstack();"; ...
  "end"]);
run(filename);
asserts.istrue(length(debugStackResult) >= 1);
% a script local function is named alone, whatever its place in the file
asserts.isequal(debugStackResult(1).name, 'scriptLocalDebugHelper');
asserts.isequal(secondStackResult(1).name, 'secondLocalDebugHelper');
rmdir(path_test, 's');
%=============================================================================
