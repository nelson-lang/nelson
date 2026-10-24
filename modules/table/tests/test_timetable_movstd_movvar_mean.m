%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% [S, M] = movstd(TT, ...) and [V, M] = movvar(TT, ...) return the moving mean as a timetable
%=============================================================================
TT = timetable(seconds((1:5)'), [1; 2; 8; 4; 5], [2; 2; 2; 6; 6]);
[S, M] = movstd(TT, 3);
assert_istrue(istimetable(S));
assert_istrue(istimetable(M));
assert_isequal(M.Properties.RowTimes, TT.Properties.RowTimes);
assert_isapprox(M.Var1, [1.5; 11 / 3; 14 / 3; 17 / 3; 4.5], 1e-12);
assert_isapprox(M.Var2, [2; 2; 10 / 3; 14 / 3; 6], 1e-12);
assert_isapprox(S.Var1, sqrt([1 / 2; 43 / 3; 28 / 3; 13 / 3; 1 / 2]), 1e-12);
%=============================================================================
[V, M] = movvar(TT, 3);
assert_istrue(istimetable(M));
assert_isapprox(V.Var1, [1 / 2; 43 / 3; 28 / 3; 13 / 3; 1 / 2], 1e-12);
assert_isapprox(M.Var1, [1.5; 11 / 3; 14 / 3; 17 / 3; 4.5], 1e-12);
%=============================================================================
