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
d = timeofday(datetime(2024, 1, 1, 12, 30, 0));
asserts.isapprox(seconds(d), 45000, 1e-6);
d = timeofday([datetime(2024, 1, 1, 0, 0, 1), datetime(2024, 1, 1, 0, 1, 0)]);
asserts.isapprox(seconds(d), [1 60], 1e-6);
asserts.istrue(isduration(d));
%=============================================================================
