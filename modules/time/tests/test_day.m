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
t = datetime(2024, 2, 29);
asserts.isequal(day(t), 29);
asserts.isequal(day(t, 'dayofyear'), 60);
asserts.isequal(day(datetime(2024, 6, 8), 'shortname'), "Sat");
asserts.isequal(day(datetime(2024, 6, 8), 'name'), "Saturday");
%=============================================================================
