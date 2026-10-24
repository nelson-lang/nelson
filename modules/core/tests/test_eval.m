%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.isequal(nargin('eval'), 1);
asserts.isequal(nargout('eval'), -1);
%=============================================================================
A = rand(3, 4);
eval('size(A)');
B = ans;
asserts.isequal(ans, [3 4]);
%=============================================================================
asserts.isequal(eval('B'), B);
%=============================================================================
try
  eval('B;');
  asserts.istrue(true);
catch
  asserts.istrue(false);
end
%=============================================================================
asserts.isequal(eval('B;'), B);
%=============================================================================
C = eval('B');
asserts.isequal(C, B);
%=============================================================================
D = eval('B;');
asserts.isequal(D, B);
%=============================================================================
expected_msg = strrep(_('Expecting %s'), '%s', _('statement list or function definition'));
asserts.checkerror('R = eval(''AA = 33'');', expected_msg);
%=============================================================================
eval('AA = 33');
asserts.isequal(AA, 33);
%=============================================================================
expected_msg = strrep(_('Expecting %s'), '%s', _('statement list or function definition'));
asserts.checkerror('R = eval(''cos = 33;'');', expected_msg);
eval('cos = 33;');
asserts.isequal(cos, 33);
%=============================================================================
asserts.checkerror('eval(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
asserts.checkerror('eval(''1'', 1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 2, 'string'));
asserts.checkerror('eval(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
%=============================================================================
asserts.isequal(eval('1 + 2'), 3);
%=============================================================================
