%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: cellfun called as a statement (nargout == 0) with a function
% that returns no output raised "Too many output arguments.". It must call
% the function with nargout 0 for each cell and require no output; a function
% that does return a value still assigns ans.
%=============================================================================
EXPECTED = [evalc('disp(1)'), evalc('disp(2)')];
R = evalc('cellfun(@(x) disp(x), {1, 2});');
assert_isequal(R, EXPECTED);
%=============================================================================
clear ans
R = evalc('cellfun(@(x) disp(x), {1, 2})');
assert_isfalse(exist('ans', 'var') == 1);
%=============================================================================
R = evalc('cellfun(@disp, {1, 2});');
assert_isequal(R, EXPECTED);
%=============================================================================
R = evalc('cellfun(@(x, y) disp(x + y), {0, 1}, {1, 1});');
assert_isequal(R, EXPECTED);
%=============================================================================
R = evalc('cellfun(@(x) error(''bad''), {1, 2}, ''ErrorHandler'', @(e, x) disp(x));');
assert_isequal(R, EXPECTED);
%=============================================================================
clear ans
evalc('cellfun(@(x) x + 1, {1, 2})');
assert_isequal(ans, [2 3]);
%=============================================================================
clear ans
evalc('cellfun(@(x) x + 1, {1, 2}, ''UniformOutput'', true)');
assert_isequal(ans, [2 3]);
%=============================================================================
clear ans
evalc('cellfun(@(x) x + 1, {1, 2}, ''UniformOutput'', false)');
assert_isequal(ans, {2, 3});
%=============================================================================
R = cellfun(@(x) x * 2, {1, 2});
assert_isequal(R, [2 4]);
%=============================================================================
MSG = 'Too many output arguments.';
assert_checkerror('R = cellfun(@(x) disp(x), {1, 2});', MSG);
%=============================================================================
