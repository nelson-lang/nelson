%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
BW = false(32, 32);
BW(10:22, 10:22) = true;
E = imerode(BW, strel('disk', 5));
asserts.isequal(sum(E(:)), 9);
BW(4, 4) = true;
O = imopen(BW, strel('disk', 3));
asserts.isfalse(O(4, 4));
asserts.istrue(O(16, 16));
I = zeros(32, 32);
I(10:22, 10:22) = 0.4;
I(14:18, 14:18) = 1;
T = imtophat(I, strel('disk', 5));
asserts.istrue(max(T(:)) > 0.5);
