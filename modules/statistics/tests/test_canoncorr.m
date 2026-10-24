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
asserts.isequal(nargin('canoncorr'), 2);
asserts.isequal(nargout('canoncorr'), 6);
%=============================================================================
X = [1 2 3; 2 1 5; 3 4 4; 4 3 8; 5 7 6; ...
  6 5 9; 7 8 11; 8 9 10; 9 11 13; 10 10 15];
Y = [3 4; 1 7; 5 5; 2 11; 9 6; 4 12; 10 10; 7 13; 12 9; 8 16];
[A, B, r, U, V, stats] = canoncorr(X, Y);
asserts.isapprox(A, [0.196684315807947 1.284241160268141; ...
  0.071879788440771 -0.966553558900145; ...
  0.043656501201557 -0.169114559908250], 1e-12);
asserts.isapprox(B, [0.180293419017243 -0.213758900644718; ...
  0.161718093723430 0.213109910670694], 1e-12);
asserts.isapprox(r, [0.991234240954053 0.936088581899088], 1e-12);
asserts.isapprox(U(:, 1), [-1.408343681387254; -1.196226151616965; ...
  -0.827558971688260; -0.528128439514858; -0.131237972346938; ...
  0.052656270184136; 0.552292953717511; 0.777200556764673; ...
  1.248613953058833; 1.460731482829122], 1e-12);
asserts.isapprox(V(:, 2), [-0.466829934556053; 0.600017598745465; ...
  -0.681237825174795; 1.238698340783524; -1.323163517082973; ...
  1.024290450164782; -0.684482775044915; 0.596123658901322; ...
  -1.325110487005045; 1.021694490268686], 1e-12);
C = corrcoef([U(:, 1), V(:, 1)]);
asserts.isapprox(C(1, 2), r(1), 1e-12);
asserts.isapprox(stats.Wilks, [0.002159810051532 0.123738166838155], 1e-12);
asserts.isequal(stats.df1, [6 2]);
asserts.isequal(stats.df2, [10 7]);
asserts.isapprox(stats.F, [34.195867136905001 24.785532988199698], 1e-12);
asserts.isapprox(stats.pF, [0.000004206983385 0.000666442787225], 1e-12);
asserts.isapprox(stats.chisq, [36.826410001551764 14.627112525306551], 1e-12);
asserts.isapprox(stats.pChisq, [0.000001903539959 0.000666442787225], 1e-12);
asserts.isequal(stats.dfe, stats.df1);
asserts.isequal(stats.p, stats.pChisq);
%=============================================================================
[A, B, r] = canoncorr(single(X), single(Y));
asserts.isequal(class(A), 'double');
asserts.isapprox(r, [0.991234240954053 0.936088581899088], 1e-12);
asserts.isequal(size(B), [2 2]);
%=============================================================================
Xdep = [1 2 2; 3 4 4; 5 8 8; 7 11 11; 9 13 13];
Ydep = [2 1; 4 2; 6 5; 8 6; 10 8];
evalc('[Adep, Bdep, rdep] = canoncorr(Xdep, Ydep);');
asserts.isequal(size(Adep), [3 2]);
asserts.isequal(size(Bdep), [2 2]);
asserts.isequal(Adep(3, :), [0 0]);
asserts.isapprox(rdep, [1.000000000000000 0.639602149066831], 1e-12);
%=============================================================================
Xconstant = [1 2; 1 2; 1 2];
Yconstant = [3 4; 5 6; 7 8];
evalc('[A0, B0, r0, U0, V0, stats0] = canoncorr(Xconstant, Yconstant);');
asserts.isequal(size(A0), [2 0]);
asserts.isequal(size(B0), [2 0]);
asserts.isequal(size(r0), [1 0]);
asserts.isequal(size(U0), [3 0]);
asserts.isequal(size(V0), [3 0]);
asserts.isequal(size(stats0.Wilks), [1 0]);
%=============================================================================
asserts.checkerror('canoncorr()', _('Not enough input arguments.'));
asserts.checkerror('canoncorr({1}, [1; 2])', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('canoncorr([1; 2], {1})', sprintf(_('%s must be a real numeric matrix.'), 'Y'));
asserts.checkerror('canoncorr([1; 2], [1; 2; 3])', sprintf(_('%s must have the same number of rows.'), 'X and Y'));
asserts.checkerror('canoncorr([1], [2])', _('X and Y must contain at least two observations and one variable.'));
asserts.checkerror('canoncorr([1; NaN], [2; 3])', sprintf(_('%s must contain finite values.'), 'X and Y'));
%=============================================================================
