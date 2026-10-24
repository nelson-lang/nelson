%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(cumprod([1 NaN 3],'omitmissing'), [1 1 3]);
asserts.isequal(cumprod([1 NaN 3],'includemissing'), [1 NaN NaN]);
asserts.isequal(cumprod(int8([1 2 3]),'omitmissing'), [int8(1) int8(2) int8(6)]);
asserts.isequal(cumprod([NaN NaN],'omitmissing'), [1 1]);
%=============================================================================
