%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(kron([true false],[true true]), [1 1 0 0]);
asserts.isequal(kron(true,true), 1);
asserts.isequal(kron(true(2,2),true(2,2)), reshape([1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1], [4 4]));
%=============================================================================
