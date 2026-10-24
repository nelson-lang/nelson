%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = logical([1 2 3; 4 5 6]);
B = logical([7; 8]);
R1 = horzcat(A, B);
R2 = [A, B];
asserts.isequal(R1, R2);
%=============================================================================
A = logical([1 2 3; 4 5 6]);
B = logical([7 8 9]);
R1 = vertcat(A, B);
R2 = [A; B];
asserts.isequal(R1, R2);
%=============================================================================
A = [1 2 3; 4 5 6];
B = [7; 8];
R1 = horzcat(A, B);
R2 = [A, B];
asserts.isequal(R1, R2);
%=============================================================================
A = [1 2 3; 4 5 6];
B = [7 8 9];
R1 = vertcat(A, B);
R2 = [A; B];
asserts.isequal(R1, R2);
%=============================================================================
A = [1 2 3i; 4 5 6];
B = [7 8 9i];
R1 = vertcat(A, B);
R2 = [A; B];
asserts.isequal(R1, R2);
%=============================================================================
A = sparse([1 2 3; 4 5 6]);
B = sparse([7; 8]);
R1 = full(horzcat(A, B));
R2 = full([A, B]);
asserts.isequal(R1, R2);
%=============================================================================
A = sparse([1 2 3i; 4 5 6]);
B = sparse([7; 8i]);
R1 = full(horzcat(A, B));
R2 = full([A, B]);
asserts.isequal(R1, R2);
%=============================================================================
A = sparse([1 2 3; 4 5 6]);
B = sparse([7 8 9]);
R1 = full(vertcat(A, B));
R2 = full([A; B]);
asserts.isequal(R1, R2);
%=============================================================================
A = sparse(single([1 + 2i 0; 0 3 - 4i]));
B = sparse(single([5 - 6i; 7]));
R = horzcat(A, B);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([1 + 2i 0 5 - 6i; 0 3 - 4i 7]), 1e-6);
%=============================================================================
A = sparse(single([1 + 2i 0]));
B = sparse(single([0 3 - 4i]));
R = vertcat(A, B);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([1 + 2i 0; 0 3 - 4i]), 1e-6);
%=============================================================================
A = sparse([1 2 3i; 4 5 6]);
B = sparse([7 8 9i]);
R1 = full(vertcat(A, B));
R2 = full([A; B]);
asserts.isequal(R1, R2);
%=============================================================================
