%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = [1 + i 2; 3 4 - i];
y = upfirdn(x, [1 -1], 1, 1, 2);
REF = [1 + i 1 - i -2; 3 1 - i -4 + i];
asserts.isapprox(y, REF, 1e-12);
%=============================================================================
y = upfirdn(uint8([1 2 3]), [1 1], 2, 2);
asserts.isequal(y(:).', [1 2 3]);
asserts.isequal(class(y), 'double');
%=============================================================================
y = upfirdn([1 2 3], [1 + i], 1, 1);
asserts.isapprox(y, [1 + i 2 + 2i 3 + 3i], 1e-12);
%=============================================================================
y = upfirdn([1; 2], [1 2; 3 4]);
asserts.isequal(y, [1 2; 5 8; 6 8]);
%=============================================================================
y = upfirdn([1 2], [1 2; 3 4]);
asserts.isequal(y, [1 2; 5 8; 6 8]);
%=============================================================================
y = upfirdn([1 2; 3 4], [1 2; 3 4]);
asserts.isequal(y, [1 4; 6 16; 9 16]);
%=============================================================================
y = upfirdn([1 2 3; 4 5 6], [1 2; 3 4], 1, 1, 2);
asserts.isequal(y, [1 5 9 9; 8 26 32 24]);
%=============================================================================
y = upfirdn([1 2 3; 4 5 6], [1 3], 1, 1, 2);
asserts.isequal(y, [1 5 9 9; 4 17 21 18]);
%=============================================================================
asserts.checkerror('upfirdn([1 2], [1], 0)', _('Value must be positive.'));
asserts.checkerror('upfirdn([1 2], [1], 1.5)', _('Value must be an integer.'));
asserts.checkerror('upfirdn([1 2], [1], 1, 0)', _('Value must be positive.'));
asserts.checkerror('upfirdn([1 2], [1], 1, Inf)', _('Value must be finite.'));
asserts.checkerror('upfirdn([1 2], [1], 1, 1, 3)', _('Dimension must be 1 or 2.'));
asserts.checkerror('upfirdn([1 2], [1], 1, 1, 0)', _('Value must be positive.'));
asserts.checkerror('upfirdn([1 2], [1], 1, 1, 1.5)', _('Value must be an integer.'));
asserts.checkerror('upfirdn([], [1 1])', _('Input signal must be nonempty.'));
asserts.checkerror('upfirdn(zeros(0, 1), [1 1])', _('Input signal must be nonempty.'));
asserts.checkerror('upfirdn([1 2], [])', _('Filter coefficients must be nonempty.'));
asserts.checkerror('upfirdn(sparse([1 2]), [1])', _('Sparse matrices are not supported.'));
asserts.checkerror('upfirdn([1 2], sparse([1]))', _('Sparse matrices are not supported.'));
asserts.checkerror('upfirdn([1 2; 3 4], [1 2 3; 4 5 6])', _('Signal X and filter H must have the same number of columns.'));
asserts.checkerror('upfirdn([1 2 3; 4 5 6], [1 2 3; 4 5 6], 1, 1, 2)', _('Signal X and filter H must have the same number of columns.'));
%=============================================================================
