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
asserts.isequal(nargin('cmdscale'), 2);
asserts.isequal(nargout('cmdscale'), 2);
%=============================================================================
X = [0 0; 1 0; 0 2; 2 2];
D = squareform(pdist(X));
[Y, e] = cmdscale(D);
asserts.isapprox(Y, [-1.238218880541527 0.171213328542167; ...
  -0.753450348148581 -0.703429152704650; ...
  0.511066081952108 1.140750393328058; ...
  1.480603146737999 -0.608534569165577], 1e-12);
asserts.isapprox(e, [4.554247641507077; 2.195752358492924; ...
  0.000000000000000; -0.000000000000000], 1e-12);
asserts.isapprox(pdist(Y), pdist(X), 1e-12);
asserts.istrue(max(abs(mean(Y, 1))) < 1e-12);
asserts.isequal(size(Y), [4 2]);
asserts.isapprox(sum(e(1:2)), 6.750000000000001, 1e-12);
%=============================================================================
[Y1, e1] = cmdscale(D, 1);
asserts.isapprox(Y1, [-1.238218880541527; -0.753450348148581; ...
  0.511066081952108; 1.480603146737999], 1e-12);
asserts.isapprox(e1, 4.554247641507077, 1e-12);
%=============================================================================
[Y3, e3] = cmdscale(D, 3);
asserts.isequal(size(Y3), [4 2]);
asserts.isapprox(Y3, Y, 1e-12);
asserts.isapprox(e3, [4.554247641507077; 2.195752358492924; ...
  0.000000000000000], 1e-12);
%=============================================================================
[Yv, ev] = cmdscale(pdist(X));
asserts.isapprox(Yv, Y, 1e-12);
asserts.isapprox(ev, e, 1e-12);
%=============================================================================
U = triu(D);
[Yu, eu] = cmdscale(U);
asserts.isapprox(Yu, Y, 1e-12);
asserts.isapprox(eu, e, 1e-12);
%=============================================================================
S = 1 - D .^ 2 / 10;
S(1:size(S, 1) + 1:end) = 1;
[Ys, es] = cmdscale(S);
asserts.isapprox(Ys, Y / sqrt(10), 1e-12);
asserts.isapprox(es, e / 10, 1e-12);
%=============================================================================
[Y0, e0] = cmdscale(0);
asserts.isequal(size(Y0), [1 0]);
asserts.isequal(e0, 0);
%=============================================================================
asserts.checkerror('cmdscale()', _('Not enough input arguments.'));
asserts.checkerror('cmdscale({1})', sprintf(_('%s must be a real numeric vector or matrix.'), 'D'));
asserts.checkerror('cmdscale([0 1; 1 NaN])', sprintf(_('%s must contain finite values.'), 'D'));
asserts.checkerror('cmdscale([0 1 2; 1 0 3])', _('D must be square or a valid distance vector.'));
asserts.checkerror('cmdscale([0 1; 2 0])', _('D must be symmetric or upper triangular.'));
asserts.checkerror('cmdscale([0 -1; -1 0])', _('D must contain nonnegative dissimilarities.'));
asserts.checkerror('cmdscale(D, 0)', _('p must be an integer scalar between 1 and the number of points.'));
asserts.checkerror('cmdscale(D, 5)', _('p must be an integer scalar between 1 and the number of points.'));
asserts.checkerror('cmdscale(D, 1.5)', _('p must be an integer scalar between 1 and the number of points.'));
%=============================================================================
