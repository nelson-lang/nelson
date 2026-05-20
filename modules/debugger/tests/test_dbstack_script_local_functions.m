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
  "function st = scriptLocalDebugHelper()"; ...
  "  st = dbstack();"; ...
  "end"]);
run(filename);
assert_istrue(length(debugStackResult) >= 1);
assert_isequal(debugStackResult(1).name, 'script_local_debug_stack>scriptLocalDebugHelper');
rmdir(path_test, 's');
%=============================================================================
