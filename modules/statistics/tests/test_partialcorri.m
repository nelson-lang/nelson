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
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('partialcorri'), -3);
asserts.isequal(nargout('partialcorri'), 2);
%=============================================================================
t = (1:10)';
X = [t, [2; 3; 5; 7; 11; 13; 17; 19; 23; 29], ...
     [5; 4; 6; 7; 9; 8; 10; 12; 13; 15]];
Y = [30 - 2 * t + [1; -1; 0; 2; -2; 1; 0; -1; 2; -2], ...
     t .^ 2 / 10 + [0; 1; -1; 1; 0; -1; 2; -2; 1; 0]];
Z = [mod(t, 3), sin(t)];
%=============================================================================
[R, P] = partialcorri(Y, X);
REF = [-0.377109889971297 -0.213876251015029 -0.073987828609532; ...
       -0.250586612526204 0.622623564422124 -0.288165467545804];
asserts.isapprox(R, REF, 1e-12);
REFP = [0.357095802285222 0.611043399460718 0.861778270015357; ...
        0.549448634057363 0.099200676618751 0.488855939687377];
asserts.isapprox(P, REFP, 1e-12);
%=============================================================================
[R, P] = partialcorri(Y, X, Z, 'Tail', 'right');
REF = [-0.561529367157574 -0.438136851237306 0.216543149042908; ...
       -0.240476120230858 0.603495325888614 -0.260815596703090];
asserts.isapprox(R, REF, 1e-12);
REFP = [0.876882334953596 0.807576023708527 0.340131115920951; ...
        0.676880480947902 0.102327752035709 0.691176216925231];
asserts.isapprox(P, REFP, 1e-12);
%=============================================================================
% Spearman partial correlation on a well-conditioned dataset: no column of X
% is a rank-permutation of another, so the ranked design is full rank. The
% previous fixture used strictly monotonic X columns whose tied ranks were
% identical, making the partial correlation of the collinear columns a 0/0
% (~1e-15 residual) and its value numerical noise. The reference below is the
% mathematically defined value (rank the data, then Pearson partial
% correlation), cross-checked against partialcorri(tiedrank(Y), tiedrank(X)).
Ys = [10 3; 4 8; 7 1; 1 9; 9 5; 5 6; 2 12; 8 4];
Xs = [3 1 5; 1 4 2; 5 2 6; 2 7 1; 6 3 4; 4 5 8; 8 8 7; 7 6 3];
S = partialcorri(Ys, Xs, 'Type', 'Spearman');
REFS = [0.734442733721541 -0.895681862740964 -0.366849272196994; ...
        -0.564331020907658 0.8735605739247 0.241824621493047];
asserts.isapprox(S, REFS, 1e-10);
%=============================================================================
Xn = X;
Xn(2, 2) = NaN;
Yn = Y;
Yn(4, 1) = NaN;
Rall = partialcorri(Yn, Xn, 'Rows', 'all');
asserts.istrue(all(isnan(Rall(:))));
Rpair = partialcorri(Yn, Xn, 'Rows', 'pairwise');
asserts.isequal(size(Rpair), [2 3]);
asserts.isfalse(all(isnan(Rpair(:))));
%=============================================================================
asserts.checkerror('partialcorri()', _('Not enough input arguments.'));
asserts.checkerror('partialcorri({1}, [1])', sprintf(_('%s must be a real numeric matrix.'), 'Y'));
asserts.checkerror('partialcorri([1], {1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('partialcorri([1], [1], {1})', sprintf(_('%s must be a real numeric matrix.'), 'Z'));
asserts.checkerror('partialcorri([1; 2], [1])', sprintf(_('%s must have the same number of rows.'), 'Y, X, and Z'));
asserts.checkerror('partialcorri([1; 2], [1; 2], ''Type'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TYPE'));
asserts.checkerror('partialcorri([1; 2], [1; 2], ''Rows'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'ROWS'));
asserts.checkerror('partialcorri([1; 2], [1; 2], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
%=============================================================================
PData = [1 2 3 4; 2 3 4 8; 3 5 6 9; 4 7 8 13; 5 11 10 17; 6 13 12 20];
Control = [1 0; 1 1; 2 1; 2 0; 3 1; 3 0];
YPart = [6 4; 5 8; 4 10; 3 13; 2 18; 1 21];
PR2 = partialcorri(YPart, PData(:, 1:3), Control);
asserts.isequal(size(PR2), [2 3]);
asserts.istrue(isfinite(PR2(1, 1)));
%=============================================================================
