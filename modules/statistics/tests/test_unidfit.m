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
asserts.isequal(nargin('unidfit'), 2);
asserts.isequal(nargout('unidfit'), 2);
%=============================================================================
x = [1 2 4 5 5];
[N, NCI] = unidfit(x);
asserts.isequal(N, 5);
asserts.isequal(NCI, [5 9]);
%=============================================================================
[N, NCI] = unidfit(x, 0.01);
asserts.isequal(N, 5);
asserts.isequal(NCI, [5 12]);
%=============================================================================
xm = [1 2; 2 4; 4 7; 5 8];
[NM, NCIM] = unidfit(xm);
asserts.isequal(NM, [5 8]);
asserts.isequal(NCIM, [5 10; 8 16]);
%=============================================================================
[N, NCI] = unidfit(single(x));
asserts.isequal(class(N), 'double');
asserts.isequal(class(NCI), 'double');
asserts.isequal(N, 5);
asserts.isequal(NCI, [5 9]);
%=============================================================================
[N, NCI] = unidfit([1 NaN 3]);
asserts.istrue(isnan(N));
asserts.istrue(all(isnan(NCI(:))));
[N, NCI] = unidfit([1 2 5], 1);
asserts.isequal(NCI, [5 5]);
[N, NCI] = unidfit([1 2 5], 0);
asserts.isequal(NCI(1), 5);
asserts.istrue(isinf(NCI(2)) && NCI(2) > 0);
%=============================================================================
asserts.checkerror('unidfit()', _('Not enough input arguments.'));
asserts.checkerror('unidfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('unidfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('unidfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('unidfit([0 1 2])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidfit([-1 1 2])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidfit([1.5 2 3])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidfit([1 Inf 2])', _('X must contain positive integer finite values.'));
asserts.checkerror('unidfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('unidfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('unidfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
%=============================================================================
