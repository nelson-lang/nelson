%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(min([1 NaN 3],[],'omitmissing'), 1);
asserts.isequal(min([1 NaN 3],[],'includemissing'), NaN);
asserts.isequal(min(int8([5 3]),[],2,'linear'), int8(3));
%=============================================================================
