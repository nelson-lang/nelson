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
filename = [path_test, '/nested_profile_case.m'];
filewrite(filename, ["function y = nested_profile_case(x)"; ...
  "  y = nested_profile_helper(x);"; ...
  "  function z = nested_profile_helper(t)"; ...
  "    z = t * 2;"; ...
  "  end"; ...
  "end"]);
addpath(path_test);
profile('on');
profileValue = nested_profile_case(21);
profile('off');
p = profile('info');
assert_isequal(profileValue, 42);
foundProfileEntry = false;
for k = 1:length(p)
  foundProfileEntry = foundProfileEntry || strcmp(p(k).FunctionName, 'nested_profile_case>nested_profile_helper');
end
assert_istrue(foundProfileEntry);
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
filename = [path_test, '/nested_profile_deeper_case.m'];
filewrite(filename, ["function y = nested_profile_deeper_case(x)"; ...
  "  factor = 2;"; ...
  "  y = nested_profile_middle(x);"; ...
  "  function a = nested_profile_middle(t)"; ...
  "    offset = 4;"; ...
  "    a = nested_profile_inner(t);"; ...
  "    function b = nested_profile_inner(v)"; ...
  "      b = v * factor + offset;"; ...
  "    end"; ...
  "  end"; ...
  "end"]);
addpath(path_test);
profile('on');
profileValue = nested_profile_deeper_case(19);
profile('off');
p = profile('info');
assert_isequal(profileValue, 42);
foundProfileEntry = false;
for k = 1:length(p)
  foundProfileEntry = foundProfileEntry || strcmp(p(k).FunctionName, 'nested_profile_deeper_case>nested_profile_middle>nested_profile_inner');
end
assert_istrue(foundProfileEntry);
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
filename = [path_test, '/nested_profile_closure_factory.m'];
filewrite(filename, ["function h = nested_profile_closure_factory(scale)"; ...
  "  factor = scale;"; ...
  "  h = @nested_profile_closure_helper;"; ...
  "  function z = nested_profile_closure_helper(t)"; ...
  "    z = t * factor;"; ...
  "  end"; ...
  "end"]);
addpath(path_test);
profile('on');
closureHandle = nested_profile_closure_factory(3);
profileValue = closureHandle(14);
profile('off');
p = profile('info');
assert_isequal(profileValue, 42);
foundProfileEntry = false;
for k = 1:length(p)
  foundProfileEntry = foundProfileEntry || strcmp(p(k).FunctionName, 'nested_profile_closure_factory>nested_profile_closure_helper');
end
assert_istrue(foundProfileEntry);
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
