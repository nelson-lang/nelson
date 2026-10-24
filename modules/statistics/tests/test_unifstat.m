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
asserts.isequal(nargin('unifstat'), 2);
asserts.isequal(nargout('unifstat'), 2);
%=============================================================================
[m, v] = unifstat(0, 1);
asserts.isequal(m, 0.5);
asserts.isequal(v, 1 / 12);
%=============================================================================
a = 1:6;
b = 2 * a;
[m, v] = unifstat(a, b);
asserts.isequal(m, [1.5 3 4.5 6 7.5 9]);
asserts.isequal(v, [1 4 9 16 25 36] / 12);
%=============================================================================
[m, v] = unifstat([1 2 3], [3 2 1]);
asserts.isequal(m(1), 2);
asserts.isequal(v(1), 1 / 3);
asserts.istrue(all(isnan(m(2:3))));
asserts.istrue(all(isnan(v(2:3))));
%=============================================================================
[m, v] = unifstat(single([0 1]), single([2 5]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isequal(m, single([1 3]));
asserts.isequal(v, single([1 / 3 4 / 3]));
%=============================================================================
m = unifstat(0, [1 2 3]);
asserts.isequal(m, [0.5 1 1.5]);
%=============================================================================
[m, v] = unifstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('unifstat(0)', _('Not enough input arguments.'));
asserts.checkerror('unifstat(0, 1, 2)', _('Too many input arguments.'));
asserts.checkerror('unifstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('unifstat({0}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('unifstat(0, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
[unifMean, unifVariance] = unifstat(0, 2);
asserts.isequal(unifMean, 1);
asserts.isapprox(unifVariance, 1 / 3, 1e-12);
%=============================================================================
