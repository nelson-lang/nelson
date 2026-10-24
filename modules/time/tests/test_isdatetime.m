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
asserts.isequal(isdatetime(datetime(2024, 1, 1)), true);
asserts.isequal(isdatetime(NaT), true);
asserts.isequal(isdatetime(days(1)), false);
asserts.isequal(isdatetime(1), false);
%=============================================================================
