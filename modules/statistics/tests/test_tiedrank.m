%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('tiedrank'), -2);
asserts.isequal(nargout('tiedrank'), 2);
%=============================================================================
A = [-2 1 3 1 4];
[R, tieadj] = tiedrank(A);
asserts.isequal(R, [1 2.5 4 2.5 5]);
asserts.isequal(tieadj, 3);
%=============================================================================
[R, tieadj] = tiedrank(A, 1);
asserts.isequal(R, [1 2.5 4 2.5 5]);
asserts.isequal(tieadj, [1; 0; 18]);
%=============================================================================
[R, tieadj] = tiedrank(A, 0, 1);
asserts.isequal(R, [1 2.5 2 2.5 1]);
asserts.isequal(tieadj, 3);
%=============================================================================
B = [4 1 4; 2 3 5; 2 1 7; 2 4 2; 1 6 4];
[R, tieadj] = tiedrank(B, 0, 1);
asserts.isapprox(R, [1 1.5 2.5; 2.333333333333333 3 2; ...
                    2.333333333333333 1.5 1; 2.333333333333333 2 1; ...
                    1 1 2.5], 1e-12);
asserts.isequal(tieadj, [12 3 3]);
%=============================================================================
C = [NaN 3 3 1];
[R, tieadj] = tiedrank(C);
asserts.istrue(isnan(R(1)));
asserts.isequal(R(2:end), [2.5 2.5 1]);
asserts.isequal(tieadj, 3);
%=============================================================================
D = cat(3, [2 2; 1 3], [4 4; 5 NaN]);
[R, tieadj] = tiedrank(D);
asserts.isequal(R(:, :, 1), [2 1; 1 2]);
asserts.isequal(R(:, :, 2), [1 1; 2 NaN]);
asserts.isequal(size(tieadj), [1 2 2]);
asserts.isequal(tieadj(:), [0; 0; 0; 0]);
%=============================================================================
[R, tieadj] = tiedrank([NaN; NaN], 1);
asserts.istrue(all(isnan(R)));
asserts.isequal(tieadj, [0; 0; 0]);
S = tiedrank(single([10 5 5]));
asserts.isequal(class(S), 'double');
asserts.isequal(S, [3 1.5 1.5]);
%=============================================================================
asserts.checkerror('tiedrank()', _('Not enough input arguments.'));
asserts.checkerror('tiedrank({1})', sprintf(_('%s must be a real numeric array.'), 'X'));
asserts.checkerror('tiedrank([1 2], 2)', sprintf(_('%s must be a logical scalar.'), 'KENDALL'));
asserts.checkerror('tiedrank([1 2], 0, 2)', sprintf(_('%s must be a logical scalar.'), 'BIDIRECTIONAL'));
%=============================================================================
