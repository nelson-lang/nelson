%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(not(true), false);
asserts.isequal(not(false), true);
asserts.isequal(~[true false; false true], [false true; true false]);
%=============================================================================
asserts.isequal(not([0 2]), [true false]);
%=============================================================================
