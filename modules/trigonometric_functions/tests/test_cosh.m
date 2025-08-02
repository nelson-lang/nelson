%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cosh'), 1);
assert_isequal(nargout('cosh'), 1);
%=============================================================================
assert_isequal(cosh(NaN), NaN);
assert_isequal(cosh(Inf), Inf);
assert_isequal(cosh(-Inf), Inf);
%=============================================================================
A = rand(100, 100);
y1 = cosh(A * i);
y2 = complex(cos(A));
assert_isapprox(y1, y2, 1e-4);
%=============================================================================
A = rand(3, 3, 3);
y1 = cosh(complex(zeros(3, 3, 3), A));
y2 = complex(cos(A));
assert_isapprox(y1, y2, 1e-4);
%=============================================================================
A = cosh(0);
REF = 1;
assert_isequal(A, REF);
%=============================================================================
A = cosh(sparse(zeros(3,3)));
REF = sparse(ones(3, 3));
assert_isequal(A, REF);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'cosh');
assert_checkerror('cosh(''a'')', msg);
%=============================================================================
