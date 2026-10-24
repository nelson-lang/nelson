%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(isregular(datetime(2024, 1, 1):days(1):datetime(2024, 1, 3)), true);
asserts.isequal(isregular([datetime(2024, 1, 1), datetime(2024, 1, 2), datetime(2024, 1, 4)]), false);
asserts.isequal(isregular([datetime(2024, 1, 1), datetime(2024, 2, 1), datetime(2024, 3, 1)], 'months'), true);
asserts.isequal(isregular(datetime(2024, 1, 1)), true);
%=============================================================================
