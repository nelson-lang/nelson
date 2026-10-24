%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% variance of complex data uses |z - mean|^2: result is real and >= 0
asserts.isapprox(var([1i 2i 3i]), 1, 1e-12);
asserts.isapprox(var([1 + 1i 2 + 2i 3 + 3i]), 2, 1e-12);
asserts.isapprox(std([1i 2i 3i]), 1, 1e-12);
asserts.isapprox(cov([1i; 2i; 3i]), 1, 1e-12);
asserts.isapprox(cov([1 + 1i 2 + 2i 3 + 3i]), 2, 1e-12);
%=============================================================================
% cov normalization argument
asserts.isapprox(cov([1 2 3], [4 8 16], 1), [2 / 3 4; 4 224 / 9], 1e-10);
asserts.isapprox(cov([1 2 3], [4 8 16]), [1 6; 6 112 / 3], 1e-10);
%=============================================================================
