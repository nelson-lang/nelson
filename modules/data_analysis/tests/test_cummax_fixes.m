%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(cummax([1 NaN 3],'omitmissing'), [1 1 3]);
asserts.isequal(cummax([1 NaN 3],'includemissing'), [1 NaN NaN]);
asserts.isequal(cummax([NaN 1 3 2],'omitmissing'), [NaN 1 3 3]);
asserts.isequal(cummax([NaN 1 3 2],'includemissing'), [NaN NaN NaN NaN]);
asserts.isequal(cummax([1 NaN 3],'omitmissing','reverse'), [3 3 3]);
asserts.isequal(cummax(int8([1 3 2]),'omitmissing'), [int8(1) int8(3) int8(3)]);
asserts.isequal(cummax([1 2;3 0],2,'omitmissing'), reshape([1 3 2 3], [2 2]));
asserts.isequal(cummax([1 NaN 3],'forward','omitmissing'), [1 1 3]);
asserts.isequal(cummax([NaN NaN],'omitmissing'), [NaN NaN]);
asserts.isequal(cummax([],'omitmissing'), zeros([0 0], 'double'));
%=============================================================================
