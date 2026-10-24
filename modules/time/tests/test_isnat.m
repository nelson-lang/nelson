%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
t = [datetime(2024, 1, 1), NaT];
asserts.isequal(isnat(t), [false true]);
asserts.isequal(isnat(NaT(2, 1)), true(2, 1));
asserts.isequal(isnat(datetime(2024, 2, 29)), false);
%=============================================================================
