%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ppval'), 2);
assert_isequal(nargout('ppval'), 1);
%=============================================================================
x = 1:4;
v = [10 20 40 80];
coefs = [0 0 10 10; 0 0 20 20; 0 0 40 40];
pp = struct('form', 'pp', 'breaks', x, 'coefs', coefs, 'pieces', 3, 'order', 4, 'dim', 1);
R = ppval(pp, [1.5 2.5]);
assert_isapprox(R, [15 30], 1e-12);
%=============================================================================
xq = [1 1.5; 2.5 4];
R = ppval(pp, xq);
REF = [10 15; 30 80];
assert_isequal(size(R), size(xq));
assert_isapprox(R, REF, 1e-12);
%=============================================================================
R = ppval(pp, [0 5]);
assert_isapprox(R, [0 120], 1e-12);
%=============================================================================
v = [10 20 40 80] + 1i * [1 2 4 8];
coefs = [0 0 10 + 1i 10 + 1i; 0 0 20 + 2i 20 + 2i; 0 0 40 + 4i 40 + 4i];
pp = struct('form', 'pp', 'breaks', x, 'coefs', coefs, 'pieces', 3, 'order', 4, 'dim', 1);
R = ppval(pp, [1.5 2.5]);
REF = [15 + 1.5i, 30 + 3i];
assert_isapprox(R, REF, 1e-12);
%=============================================================================
pp = struct('form', 'pp', 'breaks', [1 2 3], ...
  'coefs', [1 -2 1 0; 0 1 0 1], 'pieces', 2, 'order', 4, 'dim', 1);
R = ppval(pp, [1.5 2.5 3.5]);
assert_istrue(all(isfinite(R)));
assert_isequal(size(R), [1 3]);
%=============================================================================
badpp = struct('form', 'not-pp');
assert_checkerror('ppval(badpp, 1)', _('Piecewise polynomial structure expected.'));
assert_checkerror('ppval()', _('Wrong number of input arguments.'));
assert_checkerror('ppval(pp, 1, 2)', _('Wrong number of input arguments.'));
%=============================================================================
