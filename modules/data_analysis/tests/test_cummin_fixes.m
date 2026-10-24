%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(cummin([1 NaN 3],'omitmissing'), [1 1 1]);
asserts.isequal(cummin([1 NaN 3],'includemissing'), [1 NaN NaN]);
asserts.isequal(cummin([NaN 1 3 2],'omitmissing'), [NaN 1 1 1]);
asserts.isequal(cummin(int8([3 1 2]),'includemissing'), [int8(3) int8(1) int8(1)]);
asserts.isequal(cummin([1 NaN 3],'reverse','omitmissing'), [1 3 3]);
asserts.isequal(cummin([NaN NaN],'omitmissing'), [NaN NaN]);
%=============================================================================
