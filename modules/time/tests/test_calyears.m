%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
t = datetime(2024, 2, 29) + calyears(1);
asserts.isequal([year(t), month(t), day(t)], [2025 2 28]);
t = datetime(2024, 1, 1) + calyears([1 2]);
asserts.isequal(year(t), [2025 2026]);
asserts.istrue(iscalendarduration(calyears(3)));
%=============================================================================
% calyears extracts (truncated) years from a calendarDuration
assert_isequal(calyears(calmonths(13)), 1);
assert_isequal(calyears(calyears(2)), 2);
%=============================================================================
