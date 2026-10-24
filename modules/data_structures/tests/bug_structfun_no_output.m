%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: structfun called as a statement (nargout == 0) with a function
% that returns no output raised "Too many output arguments.". It must call
% the function with nargout 0 for each field and require no output; a
% function that does return a value still assigns ans.
%=============================================================================
EXPECTED = [evalc('disp(1)'), evalc('disp(2)')];
S = struct('a', 1, 'b', 2);
R = evalc('structfun(@(x) disp(x), S);');
assert_isequal(R, EXPECTED);
%=============================================================================
clear ans
R = evalc('structfun(@(x) disp(x), S)');
assert_isfalse(exist('ans', 'var') == 1);
%=============================================================================
R = evalc('structfun(@disp, S);');
assert_isequal(R, EXPECTED);
%=============================================================================
clear ans
evalc('structfun(@(x) x + 1, S)');
assert_isequal(ans, [2; 3]);
%=============================================================================
R = structfun(@(x) x * 2, S);
assert_isequal(R, [2; 4]);
%=============================================================================
assert_checkerror('R = structfun(@(x) disp(x), S);', 'Too many output arguments.');
%=============================================================================
