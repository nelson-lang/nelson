%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% months > 12 must carry over to the year with integer arithmetic
% (the old floating-point carry introduced a ~30 day offset)
asserts.isequal(datenum(2020, 14, 1), 738188);
asserts.isequal(datenum(2020, 14, 1), datenum(2021, 2, 1));
asserts.isequal(datenum(2020, 13, 15), datenum(2021, 1, 15));
asserts.isequal(datenum(2020, 25, 1), datenum(2022, 1, 1));
asserts.isequal(datenum(2020, 24, 1), datenum(2021, 12, 1));
%=============================================================================
% months < 1 are clamped to January (the expected behavior)
asserts.isequal(datenum(2020, 0, 1), datenum(2020, 1, 1));
asserts.isequal(datenum(2020, -1, 1), datenum(2020, 1, 1));
%=============================================================================
% no regression for regular months
asserts.isequal(datenum(2020, 12, 31), 738156);
%=============================================================================
