%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ppval'), 2);
asserts.isequal(nargout('ppval'), 1);
%=============================================================================
x = 1:4;
v = [10 20 40 80];
coefs = [0 0 10 10; 0 0 20 20; 0 0 40 40];
pp = struct('form', 'pp', 'breaks', x, 'coefs', coefs, 'pieces', 3, 'order', 4, 'dim', 1);
R = ppval(pp, [1.5 2.5]);
asserts.isapprox(R, [15 30], 1e-12);
%=============================================================================
xq = [1 1.5; 2.5 4];
R = ppval(pp, xq);
REF = [10 15; 30 80];
asserts.isequal(size(R), size(xq));
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = ppval(pp, [0 5]);
asserts.isapprox(R, [0 120], 1e-12);
%=============================================================================
v = [10 20 40 80] + 1i * [1 2 4 8];
coefs = [0 0 10 + 1i 10 + 1i; 0 0 20 + 2i 20 + 2i; 0 0 40 + 4i 40 + 4i];
pp = struct('form', 'pp', 'breaks', x, 'coefs', coefs, 'pieces', 3, 'order', 4, 'dim', 1);
R = ppval(pp, [1.5 2.5]);
REF = [15 + 1.5i, 30 + 3i];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
pp = struct('form', 'pp', 'breaks', [1 2 3], ...
  'coefs', [1 -2 1 0; 0 1 0 1], 'pieces', 2, 'order', 4, 'dim', 1);
R = ppval(pp, [1.5 2.5 3.5]);
asserts.istrue(all(isfinite(R)));
asserts.isequal(size(R), [1 3]);
%=============================================================================
badpp = struct('form', 'not-pp');
asserts.checkerror('ppval(badpp, 1)', _('Piecewise polynomial structure expected.'));
asserts.checkerror('ppval()', _('Not enough input arguments.'));
asserts.checkerror('ppval(pp, 1, 2)', _('Too many input arguments.'));
%=============================================================================
% generalized order: linear pieces (order 2)
pplin = struct('form', 'pp', 'breaks', [0 1 2], ...
  'coefs', [1 0; 1 1], 'pieces', 2, 'order', 2, 'dim', 1);
asserts.isapprox(ppval(pplin, 0.5), 0.5, 1e-12);
asserts.isapprox(ppval(pplin, [0.5 1.5]), [0.5 1.5], 1e-12);
%=============================================================================
% generalized order: constant pieces (order 1)
ppcst = struct('form', 'pp', 'breaks', [0 1 2 3], ...
  'coefs', [5; 6; 7], 'pieces', 3, 'order', 1, 'dim', 1);
asserts.isapprox(ppval(ppcst, [0.5 1.5 2.5]), [5 6 7], 1e-12);
%=============================================================================
% generalized order: quintic piece (order 6)
ppq = struct('form', 'pp', 'breaks', [0 1], ...
  'coefs', [1 -1 2 -3 4 5], 'pieces', 1, 'order', 6, 'dim', 1);
dx = 0.5;
REFq = ((((1 * dx - 1) * dx + 2) * dx - 3) * dx + 4) * dx + 5;
asserts.isapprox(ppval(ppq, 0.5), REFq, 1e-12);
%=============================================================================
