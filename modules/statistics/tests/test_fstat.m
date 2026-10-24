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
asserts.isequal(nargin('fstat'), 2);
asserts.isequal(nargout('fstat'), 2);
%=============================================================================
[m, v] = fstat([5 5 5], [2 3 6]);
asserts.istrue(isnan(m(1)) && isnan(v(1)));
asserts.istrue(isinf(v(2)) && v(2) > 0);
asserts.isapprox(m(2), 3, 1e-15);
asserts.isapprox(m(3), 1.5, 1e-15);
asserts.isapprox(v(3), 4.05, 1e-15);
%=============================================================================
[m, v] = fstat(single(5), single(6));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), 1.5, 1e-6);
asserts.isapprox(double(v), 4.05, 1e-5);
%=============================================================================
[m, v] = fstat([0 -1 NaN], [5 5 5]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
[m, v] = fstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('fstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('fstat({1}, 1)', sprintf(_('%s must be real numeric.'), 'V1'));
asserts.checkerror('fstat(1, {1})', sprintf(_('%s must be real numeric.'), 'V2'));
%=============================================================================
