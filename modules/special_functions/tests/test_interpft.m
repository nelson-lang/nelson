%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(isempty(which('fft')), 'fft is not available.');
asserts.isapprox(interpft([1 2 3 4], 4), [1 2 3 4], 1e-10);
asserts.isapprox(interpft([1 2 3 4], 8), [1 1.085786 2 2.5 3 3.914214 4 2.5], 1e-5);
asserts.isapprox(interpft([0 1 0 -1], 8), [0 sqrt(2) / 2 1 sqrt(2) / 2 0 -sqrt(2) / 2 -1 -sqrt(2) / 2], 1e-10);
r = interpft([1 2 3 4]', 8);
asserts.isequal(size(r), [8 1]);
%=============================================================================
