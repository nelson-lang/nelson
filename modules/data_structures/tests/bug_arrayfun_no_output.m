%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: arrayfun called as a statement (nargout == 0) with a function
% that returns no output raised "Too many output arguments.". It must call
% the function with nargout 0 for each element and require no output; a
% function that does return a value still assigns ans.
%=============================================================================
EXPECTED = [evalc('disp(1)'), evalc('disp(2)')];
R = evalc('arrayfun(@(x) disp(x), [1 2]);');
assert_isequal(R, EXPECTED);
%=============================================================================
clear ans
R = evalc('arrayfun(@(x) disp(x), [1 2])');
assert_isfalse(exist('ans', 'var') == 1);
%=============================================================================
R = evalc('arrayfun(@disp, [1 2]);');
assert_isequal(R, EXPECTED);
%=============================================================================
R = evalc('arrayfun(@(x) error(''bad''), [1 2], ''ErrorHandler'', @(e, x) disp(x));');
assert_isequal(R, EXPECTED);
%=============================================================================
clear ans
evalc('arrayfun(@(x) x + 1, [1 2])');
assert_isequal(ans, [2 3]);
%=============================================================================
clear ans
evalc('arrayfun(@(x) numel(num2str(x)), [1 22])');
assert_isequal(ans, [1 2]);
%=============================================================================
R = arrayfun(@(x) x * 2, [1 2]);
assert_isequal(R, [2 4]);
%=============================================================================
assert_checkerror('R = arrayfun(@(x) disp(x), [1 2]);', 'Too many output arguments.');
%=============================================================================
