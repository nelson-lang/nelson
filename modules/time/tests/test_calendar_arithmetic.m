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
run([modulepath('time', 'root'), '/examples/calendar_arithmetic.m']);
assert(isequal(scheduleDates, [20240131 20240229 20240331 20240430 20240531 20240630]));
assert(isequal(day(nextMonthStarts), ones(1, 6)));
%=============================================================================
