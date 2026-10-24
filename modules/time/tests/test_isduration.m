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
asserts.isequal(isduration(seconds(1)), true);
asserts.isequal(isduration(duration(1, 2, 3)), true);
asserts.isequal(isduration(datetime(2024, 1, 1)), false);
asserts.isequal(isduration(1), false);
%=============================================================================
