%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
current = max_recursion_depth();
%=============================================================================
previous = max_recursion_depth(current + 1);
asserts.isequal(previous, current);
asserts.isequal(max_recursion_depth(), current + 1);
%=============================================================================
previous = max_recursion_depth(current);
asserts.isequal(previous, current + 1);
asserts.isequal(max_recursion_depth(), current);
%=============================================================================
