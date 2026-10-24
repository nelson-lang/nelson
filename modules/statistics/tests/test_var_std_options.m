%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% w = 1: normalization by N
asserts.isapprox(var([1 2 3], 1), 2 / 3, 1e-12);
asserts.isapprox(var([1 2 3 4], 1), 1.25, 1e-12);
asserts.isapprox(std([1 2 3], 1), sqrt(2 / 3), 1e-12);
%=============================================================================
% omitnan / includenan
asserts.isequal(var([1 NaN 3], 'omitnan'), 2);
asserts.isapprox(std([1 NaN 3], 'omitnan'), sqrt(2), 1e-12);
asserts.isequal(var([1 NaN 3]), NaN);
asserts.isapprox(var([1 NaN 3], 1, 'omitnan'), 1, 1e-12);
asserts.isequal(var([NaN NaN], 'omitnan'), NaN);
%=============================================================================
% regular behavior kept
asserts.isequal(var([1 2 3]), 1);
asserts.isequal(var(5), 0);
asserts.isequal(var([1 2; 3 4]), [2 2]);
%=============================================================================
