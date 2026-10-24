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
asserts.isequal(nargin('nnmf'), -3);
asserts.isequal(nargout('nnmf'), 3);
%=============================================================================
A = [1 2 3; 4 5 6; 7 8 9; 2 3 4];
W0 = [1 0.5; 0.5 1; 1.5 0.5; 0.2 1.2];
H0 = [1 2 1; 0.5 1 2];
[W, H, D] = nnmf(A, 2, 'W0', W0, 'H0', H0, ...
  'Options', statset('MaxIter', 20, 'TolFun', 0, 'TolX', 0));
asserts.isapprox(W, [0 3.741658059074066; ...
  6.964193788221126 1.870829029537034; ...
  13.928388277184110 0.000000000000011; ...
  2.321397462245791 3.118048382561727], 1e-12);
asserts.isapprox(H, [0.502570711032417 0.574366526894191 0.646162342755964; ...
  0.267261288012995 0.534522495349991 0.801783702686987], 1e-12);
asserts.istrue(abs(D - 0.000000202286753209) < 1e-12);
asserts.isapprox(sqrt(sum(H .^ 2, 2)), [1; 1], 1e-12);
asserts.isapprox(D, norm(A - W * H, 'fro') / sqrt(numel(A)), 1e-12);
%=============================================================================
[Wm, Hm, Dm] = nnmf(A, 2, 'W0', W0, 'H0', H0, 'Algorithm', 'mult', ...
  'Options', statset('MaxIter', 20, 'TolFun', 0, 'TolX', 0));
asserts.isapprox(Wm, [1.302268167448172 2.421521487364850; ...
  3.953958917987340 4.911101886006207; ...
  10.595235117321236 3.444879816679459; ...
  1.028830995642455 4.392076593383802], 1e-12);
asserts.isapprox(Hm, [0.554759676431948 0.580292037027707 0.596240600066244; ...
  0.325811397617436 0.542266403847256 0.774463737331283], 1e-12);
asserts.isapprox(Dm, 0.195568908456632, 1e-12);
asserts.istrue(D < Dm);
%=============================================================================
Arank1 = [1 2; 3 6];
[Wr, Hr, Dr] = nnmf(Arank1, 1, 'W0', [1; 3], 'H0', [1 2], ...
  'Options', statset('MaxIter', 5, 'TolFun', 0, 'TolX', 0));
asserts.isapprox(Wr, [2.236067977499790; 6.708203932499369], 1e-12);
asserts.isapprox(Hr, [0.447213595499958 0.894427190999916], 1e-12);
asserts.isapprox(Dr, 0, 1e-12);
%=============================================================================
[Ws, Hs, Ds] = nnmf(single(Arank1), 1, 'W0', single([1; 3]), ...
  'H0', single([1 2]), 'Options', statset('MaxIter', 1));
asserts.isequal(class(Ws), 'single');
asserts.isequal(class(Hs), 'single');
asserts.isequal(class(Ds), 'single');
asserts.isapprox(double(Ws), [2.236067977499790; 6.708203932499369], 1e-6);
%=============================================================================
[Wrnd, Hrnd, Drnd] = nnmf(A, 2, 'Replicates', 2, 'Algorithm', 'mult', ...
  'Options', statset('MaxIter', 3));
asserts.isequal(size(Wrnd), [4 2]);
asserts.isequal(size(Hrnd), [2 3]);
asserts.istrue(isfinite(Drnd));
asserts.istrue(all(Wrnd(:) >= 0));
asserts.istrue(all(Hrnd(:) >= 0));
%=============================================================================
asserts.checkerror('nnmf()', _('Not enough input arguments.'));
asserts.checkerror('nnmf({1}, 2)', sprintf(_('%s must be a real numeric matrix.'), 'A'));
asserts.checkerror('nnmf([-1 0], 1)', sprintf(_('%s must contain finite nonnegative values.'), 'A'));
asserts.checkerror('nnmf([NaN 0], 1)', sprintf(_('%s must contain finite nonnegative values.'), 'A'));
asserts.checkerror('nnmf(A, 0)', sprintf(_('%s must be a positive integer scalar.'), 'k'));
asserts.checkerror('nnmf(A, 1.5)', sprintf(_('%s must be a positive integer scalar.'), 'k'));
asserts.checkerror('nnmf(A, 2, ''Algorithm'', ''bad'')', sprintf(_('Invalid %s value.'), 'Algorithm'));
asserts.checkerror('nnmf(A, 2, ''W0'', ones(3, 2))', sprintf(_('%s has incompatible size.'), 'W0'));
asserts.checkerror('nnmf(A, 2, ''H0'', ones(2, 2))', sprintf(_('%s has incompatible size.'), 'H0'));
asserts.checkerror('nnmf(A, 2, ''W0'', -ones(4, 2))', sprintf(_('%s must contain finite nonnegative values.'), 'W0'));
asserts.checkerror('nnmf(A, 2, ''Replicates'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'Replicates'));
asserts.checkerror('nnmf(A, 2, ''Options'', 1)', sprintf(_('%s must be a scalar structure.'), 'Options'));
asserts.checkerror('nnmf(A, 2, ''Unknown'', 1)', _('Unknown option.'));
%=============================================================================
