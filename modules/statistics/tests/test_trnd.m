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
asserts.isequal(nargin('trnd'), -1);
asserts.isequal(nargout('trnd'), 1);
%=============================================================================
rng(0);
R = trnd(5, 2, 3);
REF = [0.9834422103976039 -1.288489029128543 0.3575440354503709;
       1.522342872465820 1.587564139561815 -1.495287037945323];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
rng(0);
U = rand(2, 3);
E = tinv(U, 5);
rng(0);
R = trnd(5, 2, 3);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
U = rand(1, 3);
E = tinv(U, [3 5 Inf]);
rng(0);
R = trnd([3 5 Inf]);
asserts.isapprox(R, E, 1e-12);
%=============================================================================
rng(0);
R = trnd(single(5), [2 3]);
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [2 3]);
R = trnd(5, single([2 3]));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 3]);
%=============================================================================
R = trnd([0 -1 NaN], [1 3]);
asserts.istrue(all(isnan(R)));
R = trnd(5, 0, 3);
asserts.isequal(size(R), [0 3]);
%=============================================================================
asserts.checkerror('trnd([1 2], 2, 2)', _('Size mismatch on input arguments'));
asserts.checkerror('trnd(1, 1.5, 2)', _('Size must contain integer values.'));
asserts.checkerror('trnd({1})', sprintf(_('%s must be real numeric.'), 'V'));
%=============================================================================
