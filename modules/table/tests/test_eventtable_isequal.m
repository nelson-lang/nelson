%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
E = eventtable(seconds(1));
E2 = eventtable(seconds(2));
TT = timetable(seconds(1), 1);
%=============================================================================
assert_istrue(isequal(E, E));
assert_istrue(isequal(E, E, E));
assert_isfalse(isequal(E, E2));
%=============================================================================
assert_isfalse(isequal([], E));
assert_isfalse(isequal(1, E));
assert_isfalse(isequal(TT, E));
assert_isfalse(isequal(E, TT));
assert_isfalse(isequal(E, []));
assert_isfalse(isequal([], E, E));
assert_isfalse(isequal(E, E, []));
%=============================================================================
assert_istrue(isequalto(E, E));
assert_isfalse(isequalto(E, TT));
%=============================================================================
