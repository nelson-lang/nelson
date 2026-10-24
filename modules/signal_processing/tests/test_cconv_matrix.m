%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isapprox(cconv([1 2 3], [4 5 6], 2), [50 40], 1e-12);
%=============================================================================
asserts.isapprox(cconv([1 + i 2], [3 4 * i], 3), [3 + 3 * i 2 + 4 * i 8 * i], 1e-12);
%=============================================================================
asserts.isapprox(cconv([1; 2], [1; 1], 2), [3; 3], 1e-12);
asserts.isapprox(cconv([1 2], [1; 1], 2), [3 3], 1e-12);
asserts.checkerror('cconv([1 2; 3 4], [1 1], 2)', _('A and B must be vectors.'));
asserts.checkerror('cconv([1 2], [1 1], 0)', _('Convolution length must be a positive integer scalar.'));
asserts.checkerror('cconv([1 2], [1 1], 1.5)', _('Convolution length must be a positive integer scalar.'));
asserts.checkerror('cconv([1 2], [1 1], -1)', _('Convolution length must be a positive integer scalar.'));
%=============================================================================
