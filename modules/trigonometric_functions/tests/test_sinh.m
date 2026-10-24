%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('sinh'), 1);
asserts.isequal(nargout('sinh'), 1);
%=============================================================================
asserts.isequal(sinh(NaN), NaN);
asserts.isequal(sinh(Inf), Inf);
asserts.isequal(sinh(-Inf), -Inf);
%=============================================================================
A = sinh(0);
REF = 0;
asserts.isequal(A, REF);
%=============================================================================
A = sinh(-0);
REF = 0;
asserts.isequal(A, REF);
%=============================================================================
A = sinh(sparse(zeros(3, 3)));
REF = sparse(zeros(3, 3));
asserts.isequal(A, REF);
%=============================================================================
X = [0.0000 + 0.0000i 0.0000 + 1.5708i 0.0000 + 3.1416i 0.0000 + 4.7124i];
C = sinh(X);
R = real(C);
I = imag(C);
asserts.isequal(R, [0 0 0 0]);
asserts.isapprox(I, [0 1 0 -1], 1e-4);
%=============================================================================
A = zeros(1000, 1000);
C = sinh(A);
asserts.isapprox(C, A);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sinh');
asserts.checkerror('sinh(''a'')', msg);
%=============================================================================
