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
c = between(datetime(2024, 1, 15), datetime(2024, 3, 20));
asserts.isequal(split(c, 'months'), 2);
asserts.isequal(split(c, 'days'), 5);
c = between(datetime(2024, 1, 1), datetime(2025, 1, 1), 'years');
asserts.isequal(split(c, 'years'), 1);
c = between(datetime(2024, 1, 1, 1, 0, 0), datetime(2024, 1, 1, 2, 30, 0), 'days');
asserts.isequal(seconds(time(c)), 5400);
%=============================================================================
