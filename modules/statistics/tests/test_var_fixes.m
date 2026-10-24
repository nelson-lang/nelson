%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(var([1 NaN 3],'omitmissing'), 2);
asserts.isequal(var([1 NaN 3],0,'omitmissing'), 2);
asserts.isequal(var([1 NaN 3],'includemissing'), NaN);
%=============================================================================
