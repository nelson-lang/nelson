%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(prod([1 NaN 3],'omitmissing'), 3);
asserts.isequal(prod([1 NaN 3],'includemissing'), NaN);
asserts.isequal(prod(ones(2,3),'omitnan','double'), [1 1 1]);
%=============================================================================
