%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cellstr'), 1);
assert_isequal(nargout('cellstr'), 1);
%=============================================================================
A = ["Monday", "Tuesday", "Friday"];
R = cellstr(A);
REF = {'Monday', 'Tuesday', 'Friday'};
assert_isequal(R, REF);
%=============================================================================
A = {'Monday', 'Tuesday', 'Friday'};
R = cellstr(A);
REF = {'Monday', 'Tuesday', 'Friday'};
assert_isequal(R, REF);
%=============================================================================
R = cellstr({});
REF = {};
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = cellstr({1, ''e''});', _('Cell must be string scalars or character arrays.'));
%=============================================================================
R = cellstr('');
REF = {''};
assert_isequal(R, REF);
%=============================================================================
R = cellstr(['ab','cd']);
REF = {'abcd'};
assert_isequal(R, REF);
%=============================================================================
R = cellstr(['ab';'cd']);
REF = {'ab'; 'cd'};
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = cellstr([1 3]);', _(['Type not supported:' ,' ', class(3)]));
%=============================================================================
