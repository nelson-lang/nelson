%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('sin'), 1);
assert_isequal(nargout('sin'), 1);
%=============================================================================
assert_isequal(sin(NaN), NaN);
assert_isequal(sin(Inf), NaN);
assert_isequal(sin(-Inf), NaN);
%=============================================================================
A = sin(0);
REF = 0;
assert_isequal(A, REF);
%=============================================================================
A = sin(-0);
REF = 0;
assert_isequal(A, REF);
%=============================================================================
A = sin(sparse(zeros(3,3)));
REF = sparse(zeros(3, 3));
assert_isequal(A, REF);
%=============================================================================
X = [5.235987755982988157D-01;
7.853981633974482790D-01;
1.047197551196597631D+00;
1.570796326794896558D+00;
2.094395102393195263D+00;
2.356194490192344837D+00;
2.617993877991494411D+00;
3.141592653589793116D+00];
REF = [4.999999999999999503D-01;
7.071067811865475028D-01;
8.660254037844385893D-01;
1.000000000000000000D-00;
8.660254037844387616D-01;
7.071067811865475893D-01;
4.999999999999999602D-01;
1.224646799147353177D-16];
A = sin(X);
assert_isapprox(A, REF, 1e-4);
%=============================================================================
A = rand(100,100);
%=============================================================================
C = sin(-A) + sin(A);
assert_isequal(size(C(C == 0)), [10000 1]);
%=============================================================================
A = zeros(1000, 1000);
C = sin(A);
assert_isapprox(C, A);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sin');
assert_checkerror('sin(''a'')', msg);
%=============================================================================
