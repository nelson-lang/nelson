%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/152
% <-- Short Description -->
% insertion did not return expected result for empty matrix.
%=============================================================================
A = [];
A(:, :, 2) = 1:3;
assert_isequal(size(A), [ 1     3     2]);
%=============================================================================
A = [];
A(:, 1) = 1:3;
assert_isequal(size(A), [ 3     1]);
%=============================================================================
A = [];
A(:) = 3;
REF = [];
assert_isequal(A, REF);
%=============================================================================
A = [];
A(':')=3;
REF = [];
assert_isequal(A, REF);
%=============================================================================
A = ones(1, 0);
A(':')=3;
REF = zeros(1, 0);
assert_isequal(A, REF);
%=============================================================================
A = ones(1, 0);
A(':')=3;
REF = zeros(1, 0);
assert_isequal(A, REF);
%=============================================================================
A = 1;
A(:) = 3;
REF = 3;
assert_isequal(A, REF);
%=============================================================================
A = 1;
A(':') = 3;
REF = 3;
assert_isequal(A, REF);
%=============================================================================
A = [];
A(:, 1) = 3;
REF = 3;
assert_isequal(A, REF);
%=============================================================================
A = [];
A(':', 1)=3;
REF = 3;
assert_isequal(A, REF);
%=============================================================================
A = 1;
A(:, 1) = 3;
REF = 3;
assert_isequal(A, REF);
%=============================================================================
A = 1;
A(':', 1) = 3;
REF = 3;
assert_isequal(A, REF);
%=============================================================================
A = [];
A(:, 1)=1:3;
A(:, 1)=1:3;
REF = (1:3)';
assert_isequal(A, REF)
%=============================================================================
A = [];
A(':', 1)=1:3;
REF = (1:3)';
assert_isequal(A, REF)
%=============================================================================
A = [];
A(:, 1)=1:3';
REF = (1:3)';
assert_isequal(A, REF)
%=============================================================================
A = [];
A(1:3,1) = (1:3);
REF = (1:3)';
assert_isequal(A, REF)
%=============================================================================
A = [];
A(1:3,:,1) = (1:3)';
REF = (1:3)';
assert_isequal(A, REF);
%=============================================================================
A = [];
A(1:3,':',1) = (1:3)';
REF = (1:3)';
assert_isequal(A, REF);
%=============================================================================
A = [];
A(1:3,:) = (1:3)';
REF = (1:3)';
assert_isequal(A, REF);
%=============================================================================
A = [];
A(1:3, ':') = (1:3)';
REF = (1:3)';
assert_isequal(A, REF);
%=============================================================================
A = [];
A(1:3,1) = (1:3)';
REF = (1:3)';
assert_isequal(A, REF)
%=============================================================================
A = 1:3;
assert_checkerror('A(1) = ones(1,0);', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
A = [];
assert_checkerror('A(1:3,1:3,1) = (1:3)'';', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
A = [];
assert_checkerror('A(1:3,:,1) = (1:4)'';', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
A = [];
assert_checkerror('A(1:3,'':'',1) = (1:4)'';', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
A = 1:3;
assert_checkerror('A(2) = single([])', _('Empty matrix of type double expected.'));
%=============================================================================
A = 1:3;
assert_checkerror('A(2, 1) = single([])', _('Empty matrix of type double expected.'));
%=============================================================================
A = [];
A(false) = zeros(3, 0);
assert_isequal(A, []);
%=============================================================================
A = [2 3];
A([false, false]) = zeros(3, 0);
assert_isequal(A, [2 3]);
%=============================================================================