%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('cellstr'), 1);
asserts.isequal(nargout('cellstr'), 1);
%=============================================================================
A = ["Monday", "Tuesday", "Friday"];
R = cellstr(A);
REF = {'Monday', 'Tuesday', 'Friday'};
asserts.isequal(R, REF);
%=============================================================================
A = {'Monday', 'Tuesday', 'Friday'};
R = cellstr(A);
REF = {'Monday', 'Tuesday', 'Friday'};
asserts.isequal(R, REF);
%=============================================================================
R = cellstr({});
REF = {};
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('R = cellstr({1, ''e''});', _('Cell must be string scalars or character arrays.'));
%=============================================================================
R = cellstr('');
REF = {''};
asserts.isequal(R, REF);
%=============================================================================
R = cellstr(['ab', 'cd']);
REF = {'abcd'};
asserts.isequal(R, REF);
%=============================================================================
R = cellstr(['ab'; 'cd']);
REF = {'ab'; 'cd'};
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('R = cellstr([1 3]);', sprintf(_('Type not supported: %s'), class(3)));
%=============================================================================
asserts.isequal(cellstr(['ab'; 'cd']), {'ab'; 'cd'});
asserts.isequal(cellstr(''), {''});
asserts.isequal(cellstr({}), {});
%=============================================================================
