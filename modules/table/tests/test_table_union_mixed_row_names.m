%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% union / setxor of tables when only one of them has row names follow
% vertical concatenation: the other rows get Row<k> names.
A = table([3; 1], 'RowNames', {'r1', 'r2'});
B = table(5);
C = union(A, B);
asserts.isequal(C.Var1, [1; 3; 5]);
asserts.isequal(C.Properties.RowNames, {'r2'; 'r1'; 'Row3'});
%=============================================================================
C = union(B, A);
asserts.isequal(C.Var1, [1; 3; 5]);
asserts.isequal(C.Properties.RowNames, {'r2'; 'r1'; 'Row1'});
%=============================================================================
C = union(A, B, 'stable');
asserts.isequal(C.Var1, [3; 1; 5]);
asserts.isequal(C.Properties.RowNames, {'r1'; 'r2'; 'Row3'});
%=============================================================================
C = setxor(A, B);
asserts.isequal(C.Var1, [1; 3; 5]);
asserts.isequal(C.Properties.RowNames, {'r2'; 'r1'; 'Row3'});
%=============================================================================
C = setxor(A, table([5; 3]));
asserts.isequal(C.Var1, [1; 5]);
asserts.isequal(C.Properties.RowNames, {'r2'; 'Row2'});
%=============================================================================
