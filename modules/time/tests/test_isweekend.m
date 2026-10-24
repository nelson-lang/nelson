%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(isweekend(datetime(2024, 6, 8)), true);
asserts.isequal(isweekend(datetime(2024, 6, 10)), false);
asserts.isequal(isweekend([datetime(2024, 6, 8), datetime(2024, 6, 9), datetime(2024, 6, 10)]), [true true false]);
%=============================================================================
