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
[tf, rows] = containsrange(TT, seconds([1.5; 2.5]));
asserts.istrue(tf);
asserts.isequal(rows, logical([0; 1; 0]));
[tf, rows] = containsrange(TT, seconds([0; 4]));
asserts.isfalse(tf);
asserts.isequal(rows, logical([1; 1; 1]));
%=============================================================================
