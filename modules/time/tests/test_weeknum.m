%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(weeknum(datetime(2024, 1, 1)), 1);
asserts.isequal(weeknum(datetime(2024, 1, 8)), 2);
asserts.isequal(weeknum([datetime(2024, 1, 1), datetime(2024, 1, 15)]), [1 3]);
%=============================================================================
