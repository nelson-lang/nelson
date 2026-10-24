%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[y, idx] = hampel([1 1; 1 10; 1 1], 1, 2);
asserts.isequal(y, [1 1; 1 1; 1 1]);
asserts.isequal(idx, [false false; false true; false false]);
%=============================================================================
[y, idx, xmedian, xsigma] = hampel([1 1; 1 10; 1 1; 4 4], 1, 2);
asserts.isequal(y, [1 1; 1 1; 1 1; 4 4]);
asserts.isequal(idx, [false false; false true; false false; false false]);
asserts.isapprox(xmedian, [1 5.5; 1 1; 1 4; 2.5 2.5], 1e-12);
asserts.isapprox(xsigma, [0 6.67170998327521; 0 0; 0 4.44780665551681; ...
  2.2239033277584 2.2239033277584], 1e-12);
%=============================================================================
