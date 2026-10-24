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
asserts.isequal(months(datetime(2024, 1, 1), datetime(2024, 3, 1)), 2);
asserts.isequal(months(datetime(2024, 1, 31), datetime(2024, 3, 30)), 1);
asserts.isequal(months([datetime(2024, 1, 1), datetime(2024, 2, 1)], [datetime(2024, 2, 1), datetime(2024, 4, 1)]), [1 2]);
%=============================================================================
