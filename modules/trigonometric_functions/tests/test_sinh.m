%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('sinh'), 1);
assert_isequal(nargout('sinh'), 1);
%=============================================================================
assert_isequal(sinh(NaN), NaN);
assert_isequal(sinh(Inf), Inf);
assert_isequal(sinh(-Inf), -Inf);
%=============================================================================
A = sinh(0);
REF = 0;
assert_isequal(A, REF);
%=============================================================================
A = sinh(-0);
REF = 0;
assert_isequal(A, REF);
%=============================================================================
A = sinh(sparse(zeros(3,3)));
REF = sparse(zeros(3, 3));
assert_isequal(A, REF);
%=============================================================================
X = [0.0000 + 0.0000i   0.0000 + 1.5708i   0.0000 + 3.1416i   0.0000 + 4.7124i];
C = sinh(X);
R = real(C);
I = imag(C);
assert_isequal(R, [0 0 0 0]);
assert_isapprox(I, [0 1 0 -1], 1e-4);
%=============================================================================
A = zeros(1000, 1000);
C = sinh(A);
assert_isapprox(C, A);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sinh');
assert_checkerror('sinh(''a'')', msg);
%=============================================================================
