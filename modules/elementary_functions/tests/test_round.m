%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('round'), 2)
asserts.isequal(nargout('round'), 1)
%=============================================================================
A = [2.3 3.8 5.5 -2.3 -3.8 -5.5];
REF = [2 4 6 -2 -4 -6];
R = round(A);
asserts.isequal(R, REF);
%=============================================================================
A = single([2.3 3.8 5.5 -2.3 -3.8 -5.5]);
R = round(A);
REF = single([2 4 6 -2 -4 -6]);
asserts.isequal(R, REF);
%=============================================================================
A = [2.3 3.8 5.5 -2.3 -3.8 -5.5];
REF = [2 4 6 -2 -4 -6];
R = round(sparse(A));
REF = sparse(REF);
asserts.isequal(R, REF);
%=============================================================================
A = sparse(single([2.3 + 4.6i 0; -2.3 - 4.6i 3.5]));
R = round(A);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
REF = single([2 + 5i 0; -2 - 5i 4]);
asserts.isapprox(full(R), REF, 1e-6);
%=============================================================================
A = complex([2.3 3.8 5.5 -2.3 -3.8 -5.5], [2.3 3.8 5.5 -2.3 -3.8 -5.5] * 2);
R = round(A);
REF = [2.0000 + 5.0000i, 4.0000 + 8.0000i, 6.0000 + 11.0000i, -2.0000 - 5.0000i, -4.0000 - 8.0000i, -6.0000 - 11.0000i];
asserts.isequal(R, REF);
%=============================================================================
A = round(int8(3));
R = class(A);
asserts.isequal(R, 'int8');
%=============================================================================
asserts.isequal(round(NaN), NaN);
asserts.isequal(round(Inf), Inf);
asserts.isequal(round(-Inf), -Inf);
asserts.isequal(round(zeros(0, 3)), zeros(0, 3));
%=============================================================================
asserts.isequal(round([2.3 3.8 5.5 -2.3 -3.8 -5.5]), [2 4 6 -2 -4 -6]);
%=============================================================================
% round with N digits on sparse matrices (stays sparse)
s = sparse([1.234 0; 0 2.567]);
r = round(s, 1);
assert_istrue(issparse(r));
assert_isequal(full(r), [1.2 0; 0 2.6]);
assert_isequal(full(round(sparse([123.456 0; 0 789]), -1)), [120 0; 0 790]);
assert_isequal(full(round(sparse([1.23456 0]), 3, 'significant')), [1.23 0]);
%=============================================================================
