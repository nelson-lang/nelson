%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An `asserts.*` call written as a statement is a call, not an expression whose
% value is displayed, so it asks for no output and a failing assertion raises
% whether or not the statement ends with a semicolon. Tests write assertions
% both ways, and the unterminated form used to return the outcome into `ans`
% and let execution continue.
%=============================================================================
% Written exactly as a test writes them: bare statements, no semicolon.
silent = {};
try
  asserts.istrue(false)
  silent{end + 1} = 'istrue';
catch
end
try
  asserts.isfalse(true)
  silent{end + 1} = 'isfalse';
catch
end
try
  asserts.isequal(1, 2)
  silent{end + 1} = 'isequal';
catch
end
try
  asserts.isapprox(1, 2, 1e-10)
  silent{end + 1} = 'isapprox';
catch
end
try
  asserts.checkerror('1 + 1', 'no error is raised here')
  silent{end + 1} = 'checkerror';
catch
end
if ~isempty(silent)
  error('Nelson:asserts:testFailure', [_('unterminated asserts statements did not raise: '), strjoin(silent, ', ')]);
end
%=============================================================================
% The terminated form keeps raising, with the same identifier and message.
assert_checkerror('asserts.istrue(false)', _('Assertion failed: found false entry in condition = false.'), 'Nelson:asserts:assertionFailed');
assert_checkerror('asserts.istrue(false);', _('Assertion failed: found false entry in condition = false.'), 'Nelson:asserts:assertionFailed');
%=============================================================================
assert_checkerror('asserts.isfalse(true)', _('Assertion failed: found true entry in condition = true.'), 'Nelson:asserts:assertionFailed');
assert_checkerror('asserts.isfalse(true);', _('Assertion failed: found true entry in condition = true.'), 'Nelson:asserts:assertionFailed');
%=============================================================================
assert_checkerror('asserts.isequal(1, 2)', _('Assertion failed: expected (2) and computed (1) values are different.'), 'Nelson:asserts:assertionFailed');
assert_checkerror('asserts.isequal(1, 2);', _('Assertion failed: expected (2) and computed (1) values are different.'), 'Nelson:asserts:assertionFailed');
%=============================================================================
% A passing assertion statement produces nothing, as the unqualified form does:
% no output means no `ans`.
clear ans
assert_istrue(true)
assert_isfalse(exist('ans', 'var') == 1);
clear ans
asserts.istrue(true)
assert_isfalse(exist('ans', 'var') == 1);
%=============================================================================
% Asking for an output still gets one, in every position where a value is used.
r = asserts.istrue(false);
assert_isequal(r, false);
[r, msg] = asserts.istrue(false);
assert_isequal(r, false);
assert_isequal(msg, _('Assertion failed: found false entry in condition = false.'));
if asserts.isequal(1, 1)
  ok = true;
else
  ok = false;
end
assert_istrue(ok);
assert_isequal(double(asserts.isequal(1, 2)), 0);
%=============================================================================
