%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(cumsum([1 NaN 3],'omitmissing'), [1 1 4]);
asserts.isequal(cumsum([1 NaN 3],'includemissing'), [1 NaN NaN]);
asserts.isequal(cumsum([1 NaN 3],'reverse','omitmissing'), [4 3 3]);
asserts.isequal(cumsum([1 2;NaN 4],1,'omitmissing'), reshape([1 1 2 6], [2 2]));
asserts.isequal(cumsum([1 2;NaN 4],2,'includemissing'), reshape([1 NaN 3 NaN], [2 2]));
asserts.isequal(cumsum(single([1 NaN 3]),'omitmissing'), single([1 1 4]));
asserts.isequal(cumsum(int8([1 2 3]),'omitmissing'), [int8(1) int8(3) int8(6)]);
asserts.isequal(cumsum([NaN NaN],'omitmissing'), [0 0]);
asserts.isequal(cumsum([],'omitmissing'), zeros([0 0], 'double'));
%=============================================================================
