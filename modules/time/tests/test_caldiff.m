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
t = [datetime(2024, 1, 1), datetime(2024, 2, 1), datetime(2024, 4, 1)];
c = caldiff(t, 'months');
asserts.isequal(split(c, 'months'), [1 2]);
t = [datetime(2024, 1, 1); datetime(2024, 1, 3); datetime(2024, 1, 6)];
c = caldiff(t, 'days', 1);
asserts.isequal(split(c, 'days'), [2; 3]);
%=============================================================================
