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
asserts.isequal(nargin('lognstat'), 2);
asserts.isequal(nargout('lognstat'), 2);
%=============================================================================
[m, v] = lognstat(0, 1);
asserts.isapprox(m, 1.6487212707001282, 1e-15);
asserts.isapprox(v, 4.670774270471604, 1e-14);
%=============================================================================
[m, v] = lognstat([0 1], [0 2]);
asserts.isapprox(m, [1 20.085536923187668], 1e-13);
asserts.isapprox(v, [0 21623.03700131398], 1e-9);
%=============================================================================
[m, v] = lognstat(single([0 1]), single([1 0]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [1.6487212707001282 exp(1)], 1e-6);
asserts.isapprox(double(v), [4.670774270471604 0], 1e-5);
%=============================================================================
[m, v] = lognstat([0 1 NaN], [1 -1 1]);
asserts.isapprox(m(1), 1.6487212707001282, 1e-15);
asserts.istrue(all(isnan(m(2:3))));
asserts.istrue(all(isnan(v(2:3))));
%=============================================================================
[m, v] = lognstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('lognstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('lognstat({0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('lognstat(0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
