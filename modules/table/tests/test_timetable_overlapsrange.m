%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [10; 20; 30], 'VariableNames', {'A'});
[tf, rows] = overlapsrange(TT, seconds([2.5; 4]));
asserts.istrue(tf);
asserts.isequal(rows, logical([0; 0; 1]));
[tf, rows] = overlapsrange(TT, seconds([4; 5]));
asserts.isfalse(tf);
asserts.isequal(rows, logical([0; 0; 0]));
%=============================================================================
