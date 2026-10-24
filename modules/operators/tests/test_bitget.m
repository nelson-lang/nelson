%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(bitget([0 1 2 3 4 5], [1 1 2 2 3 3]), [0 1 1 1 1 1]);
asserts.isequal(bitget(uint8([1 2 3]), 1), uint8([1 0 1]));
asserts.isequal(bitget(int8(-1), 8), int8(1));
asserts.isequal(bitget(255, 8, 'uint8'), 1);
asserts.checkerror('bitget(1, 0)', _('Bit position out of range.'));
asserts.checkerror('bitget(uint8(1), 9)', _('Bit position out of range.'));
asserts.checkerror('bitget([1 2], [1 2 3])', _('Inputs must have the same size.'));
%=============================================================================
