%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Only some tables have row names: the rows of the others get Row<k>, k
% being the row number in the result.
A = table([3; 1], 'RowNames', {'r1', 'r2'});
B = table(5);
R = [A; B];
asserts.isequal(R.Var1, [3; 1; 5]);
asserts.isequal(R.Properties.RowNames, {'r1'; 'r2'; 'Row3'});
%=============================================================================
R = [B; A];
asserts.isequal(R.Var1, [5; 3; 1]);
asserts.isequal(R.Properties.RowNames, {'Row1'; 'r1'; 'r2'});
%=============================================================================
R = vertcat(B, A, B);
asserts.isequal(R.Var1, [5; 3; 1; 5]);
asserts.isequal(R.Properties.RowNames, {'Row1'; 'r1'; 'r2'; 'Row4'});
%=============================================================================
% A default name already given to another row is made unique.
C = table([7; 8], 'RowNames', {'Row3', 'x'});
R = [C; B];
asserts.isequal(R.Properties.RowNames, {'Row3'; 'x'; 'Row3_1'});
%=============================================================================
% Row names must stay unique across the tables.
asserts.checkerror('R = [A; A];', _('Duplicate table row name: ''r1''.'));
asserts.checkerror('R = [A; table(9, ''RowNames'', {''r2''})];', _('Duplicate table row name: ''r2''.'));
%=============================================================================
