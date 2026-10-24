%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [5 12 33 39 78 90 95 107];
R = issorted(A);
asserts.istrue(R);
%=============================================================================
A = [5 33 12 39 78 90 95 107];
R = issorted(A);
asserts.isfalse(R);
%=============================================================================
A = magic(5);
R = issorted(A, 2, 'descend');
asserts.isfalse(R);
%=============================================================================
B = sort(A, 2, 'descend');
R = issorted(B, 2, 'descend');
asserts.istrue(R);
%=============================================================================
A = magic(5);
B = sort(A, 'descend');
R = issorted(A, 'descend');
asserts.isfalse(R);
R = issorted(B, 'descend');
asserts.istrue(R);
%=============================================================================
R = issorted([1 2 3; 4 5 6; 7 8 9], 'rows');
asserts.istrue(R);
%=============================================================================
R = issorted([4 2 3; 1 5 6; 7 8 9], 'rows');
asserts.isfalse(R);
%=============================================================================
A = magic(5);
B = sort(A, 'descend');
R = issorted(A, 'rows');
asserts.isfalse(R);
%=============================================================================
R = issorted(B, 'descend');
asserts.istrue(R);
%=============================================================================
rng('default');
A = rand(2, 2, 2);
B = sort(A);
R = issorted(A);
asserts.isfalse(R);
R = issorted(B);
asserts.istrue(R);
R = issorted(A, 1, 'ascend');
asserts.isfalse(R);
R = issorted(B, 1, 'ascend');
asserts.istrue(R);
%=============================================================================
asserts.istrue(issorted([1 2 3]));
asserts.isfalse(issorted([1 3 2]));
asserts.istrue(issorted([1 2 3; 4 5 6; 7 8 9], 'rows'));
asserts.isfalse(issorted([4 2 3; 1 5 6; 7 8 9], 'rows'));
%=============================================================================
