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
filename = [path_test, '/nested_debug_stack_case.m'];
filewrite(filename, ["function st = nested_debug_stack_case()"; ...
  "  st = nested_debug_helper();"; ...
  "  function out = nested_debug_helper()"; ...
  "    out = dbstack();"; ...
  "  end"; ...
  "end"]);
addpath(path_test);
stackResult = nested_debug_stack_case();
assert_istrue(length(stackResult) >= 1);
assert_isequal(stackResult(1).name, 'nested_debug_stack_case>nested_debug_helper');
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
filename = [path_test, '/nested_debug_deeper_case.m'];
filewrite(filename, ["function st = nested_debug_deeper_case()"; ...
  "  st = nested_debug_middle();"; ...
  "  function out = nested_debug_middle()"; ...
  "    out = nested_debug_inner();"; ...
  "    function innerOut = nested_debug_inner()"; ...
  "      innerOut = dbstack();"; ...
  "    end"; ...
  "  end"; ...
  "end"]);
addpath(path_test);
stackResult = nested_debug_deeper_case();
assert_istrue(length(stackResult) >= 1);
assert_isequal(stackResult(1).name, 'nested_debug_deeper_case>nested_debug_middle>nested_debug_inner');
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
filename = [path_test, '/nested_debug_closure_factory.m'];
filewrite(filename, ["function h = nested_debug_closure_factory()"; ...
  "  h = @nested_debug_closure_helper;"; ...
  "  function out = nested_debug_closure_helper()"; ...
  "    out = dbstack();"; ...
  "  end"; ...
  "end"]);
addpath(path_test);
closureHandle = nested_debug_closure_factory();
stackResult = closureHandle();
assert_istrue(length(stackResult) >= 1);
assert_isequal(stackResult(1).name, 'nested_debug_closure_factory>nested_debug_closure_helper');
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
