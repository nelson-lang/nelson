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
asserts.isequal(nargin('nbinstat'), 2);
asserts.isequal(nargout('nbinstat'), 2);
%=============================================================================
[m, v] = nbinstat(3, 0.4);
asserts.isapprox(m, 4.5, 1e-15);
asserts.isapprox(v, 11.25, 1e-15);
%=============================================================================
[m, v] = nbinstat([1 3 2.5], [0.5 0.4 1]);
asserts.isapprox(m, [1 4.5 0], 1e-15);
asserts.isapprox(v, [2 11.25 0], 1e-15);
%=============================================================================
[m, v] = nbinstat(single([1 3]), single([0.5 0.4]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [1 4.5], 1e-6);
asserts.isapprox(double(v), [2 11.25], 1e-6);
%=============================================================================
[m, v] = nbinstat([3 0 -1 NaN], [0 0.4 0.4 0.4]);
asserts.istrue(isinf(m(1)) && m(1) > 0);
asserts.istrue(isinf(v(1)) && v(1) > 0);
asserts.istrue(all(isnan(m(2:4))));
asserts.istrue(all(isnan(v(2:4))));
%=============================================================================
[m, v] = nbinstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('nbinstat(1)', _('Not enough input arguments.'));
asserts.checkerror('nbinstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('nbinstat({1}, 0.4)', _('R must be a positive numeric array.'));
asserts.checkerror('nbinstat(1, {0.4})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
