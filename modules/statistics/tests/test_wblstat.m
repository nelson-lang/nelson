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
asserts.isequal(nargin('wblstat'), 2);
asserts.isequal(nargout('wblstat'), 2);
%=============================================================================
[m, v] = wblstat(2, 3);
asserts.isapprox(m, 1.785959023138499, 1e-14);
asserts.isapprox(v, 0.4213315394739157, 1e-14);
%=============================================================================
[m, v] = wblstat([1 2], [1 2]);
asserts.isapprox(m, [1 1.772453850905516], 1e-14);
asserts.isapprox(v, [1 0.8584073464102064], 1e-14);
%=============================================================================
[m, v] = wblstat(single([1 2]), single([1 2]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [1 1.772453850905516], 1e-6);
asserts.isapprox(double(v), [1 0.8584073464102064], 1e-6);
%=============================================================================
[m, v] = wblstat([1 0 -1 NaN], [1 1 1 1]);
asserts.isapprox(m(1), 1, 1e-15);
asserts.istrue(all(isnan(m(2:4))));
asserts.istrue(all(isnan(v(2:4))));
[m, v] = wblstat([1 1], [0 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
[m, v] = wblstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('wblstat(1)', _('Not enough input arguments.'));
asserts.checkerror('wblstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('wblstat({1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('wblstat(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
