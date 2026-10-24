%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% ordered comparisons of complex values must use the real part only
%=============================================================================
asserts.istrue((1 + 2i) < (2 + 1i));
asserts.isfalse((1 + 2i) < (1 + 3i));
asserts.isfalse((2 + 1i) < (1 + 5i));
asserts.istrue((2 + 1i) > (1 + 5i));
asserts.isfalse((1 + 2i) > (2 + 1i));
asserts.istrue((1 + 2i) <= (1 + 3i));
asserts.istrue((1 + 2i) >= (1 + 3i));
asserts.isfalse((2 + 1i) <= (1 + 5i));
asserts.istrue((1 + 2i) >= (1 - 2i));
%=============================================================================
% single complex
asserts.istrue(single(1 + 2i) < single(2 + 1i));
asserts.isfalse(single(1 + 2i) < single(1 + 3i));
%=============================================================================
% sparse complex
A = sparse([1 + 2i, 0]);
B = sparse([2 + 1i, 0]);
asserts.isequal(full(A < B), logical([1 0]));
asserts.isequal(full(A > B), logical([0 0]));
%=============================================================================
