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
filename = [path_test, '/script_local_profile_case.m'];
filewrite(filename, ["profileValue = scriptLocalProfileHelper(21);"; ...
  "function y = scriptLocalProfileHelper(x)"; ...
  "  y = x * 2;"; ...
  "end"]);
profile('on');
run(filename);
profile('off');
p = profile('info');
assert_isequal(profileValue, 42);
foundProfileEntry = false;
for k = 1:length(p)
  foundProfileEntry = foundProfileEntry || strcmp(p(k).FunctionName, 'script_local_profile_case>scriptLocalProfileHelper');
end
assert_istrue(foundProfileEntry);
rmdir(path_test, 's');
%=============================================================================
