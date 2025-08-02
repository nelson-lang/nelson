%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('eval'), 1);
assert_isequal(nargout('eval'), -1);
%=============================================================================
A = rand(3, 4);
eval('size(A)');
B = ans;
assert_isequal(ans, [3 4]);
%=============================================================================
assert_isequal(eval('B'), B);
%=============================================================================
try
  eval('B;');
  assert_istrue(true);
catch
  assert_istrue(false);
end
%=============================================================================
assert_isequal(eval('B;'), B);
%=============================================================================
C = eval('B');
assert_isequal(C, B);
%=============================================================================
D = eval('B;');
assert_isequal(D, B);
%=============================================================================
expected_msg = sprintf(_('Expecting %s'), _('statement list or function definition'));
assert_checkerror('R = eval(''AA = 33'');', expected_msg);
%=============================================================================
eval('AA = 33');
assert_isequal(AA, 33);
%=============================================================================
expected_msg = sprintf(_('Expecting %s'), _('statement list or function definition'));
assert_checkerror('R = eval(''cos = 33;'');', expected_msg);
eval('cos = 33;');
assert_isequal(cos, 33);
%=============================================================================
assert_checkerror('eval(1);', _('#1 string expected.'));
assert_checkerror('eval(''1'', 1);', _('#2 string expected.'));
assert_checkerror('eval(1);', _('#1 string expected.'));
%=============================================================================

