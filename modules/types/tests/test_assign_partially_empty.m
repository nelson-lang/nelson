%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% ':' spans the current extent of a partially empty array (2x0, 0x2, ...)
x = zeros(2, 0); x(:, 1) = 1;
asserts.isequal(x, [1; 1]);
x = zeros(2, 0); x(:, 1) = [1; 2];
asserts.isequal(x, [1; 2]);
x = zeros(2, 0); x(:, 1) = [1 2];
asserts.isequal(x, [1; 2]);
x = zeros(2, 0); x(:, 2) = 1;
asserts.isequal(x, [0 1; 0 1]);
x = zeros(2, 0); x(:, 2) = [1; 2];
asserts.isequal(x, [0 1; 0 2]);
x = zeros(2, 0); x(:, end + 1) = 1;
asserts.isequal(x, [1; 1]);
x = zeros(2, 0); x(:, 1, 2) = 1;
asserts.isequal(x, reshape([0 0 1 1], 2, 1, 2));
assert_checkerror('x = zeros(2, 0); x(:, 1) = [1; 2; 3];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('x = zeros(2, 0); x(:, :) = [1; 2];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('x = zeros(2, 0); x(1, :) = [1 2];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
% a ':' over a zero extent assigns nothing but the other subscripts still grow
x = zeros(2, 0); x(:, :) = 1;
asserts.isequal(size(x), [2 0]);
x = zeros(2, 0); x(:) = 1;
asserts.isequal(size(x), [2 0]);
x = zeros(2, 0); x(1, :) = 1;
asserts.isequal(size(x), [2 0]);
x = zeros(2, 0); x(3, :) = 1;
asserts.isequal(size(x), [3 0]);
x = zeros(2, 0); x(:, :, 2) = 1;
asserts.isequal(size(x), [2 0 2]);
x = zeros(0, 2); x(:, 1) = 1;
asserts.isequal(size(x), [0 2]);
x = zeros(0, 2); x(:, 3) = 1;
asserts.isequal(size(x), [0 3]);
x = zeros(0, 2); x(:, :) = 1;
asserts.isequal(size(x), [0 2]);
x = zeros(0, 2); x(:) = 1;
asserts.isequal(size(x), [0 2]);
x = zeros(1, 0); x(1, :) = 1;
asserts.isequal(size(x), [1 0]);
x = zeros(1, 0); x(2, :) = 1;
asserts.isequal(size(x), [2 0]);
x = zeros(0, 1); x(:, :) = 1;
asserts.isequal(size(x), [0 1]);
x = zeros(0, 1); x(:, 2) = 1;
asserts.isequal(size(x), [0 2]);
x = zeros(2, 0, 3); x(:, :, 1) = 1;
asserts.isequal(size(x), [2 0 3]);
%=============================================================================
x = zeros(0, 2); x(1, :) = 1;
asserts.isequal(x, [1 1]);
x = zeros(0, 2); x(1, :) = [1 2];
asserts.isequal(x, [1 2]);
x = zeros(0, 2); x(2, :) = 1;
asserts.isequal(x, [0 0; 1 1]);
x = zeros(0, 2); x(end + 1, :) = 1;
asserts.isequal(x, [1 1]);
assert_checkerror('x = zeros(0, 2); x(1, :) = [1 2 3];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
x = zeros(2, 0); x(1, 1) = 1;
asserts.isequal(x, [1; 0]);
x = zeros(2, 0); x(3, 1) = 1;
asserts.isequal(x, [0; 0; 1]);
x = zeros(2, 0); x(1, 3) = 1;
asserts.isequal(x, [0 0 1; 0 0 0]);
x = zeros(1, 0); x(:, 1) = 1;
asserts.isequal(x, 1);
x = zeros(1, 0); x(:, 2) = 1;
asserts.isequal(x, [0 1]);
x = zeros(0, 1); x(1, :) = 1;
asserts.isequal(x, 1);
x = zeros(0, 1); x(2, :) = 1;
asserts.isequal(x, [0; 1]);
x = zeros(2, 0, 3); x(:, 1, :) = 1;
asserts.isequal(x, ones(2, 1, 3));
x = zeros(2, 0, 3); x(:, 1, 1) = 1;
asserts.isequal(x, reshape([1 1 0 0 0 0], 2, 1, 3));
x = zeros(0, 0, 3); x(1, 1, :) = 1;
asserts.isequal(x, ones(1, 1, 3));
%=============================================================================
% a 0x0 array takes the extents of ':' from the value
x = []; x(:, 1) = 1;
asserts.isequal(x, 1);
x = []; x(:, 2) = [1 2 3];
asserts.isequal(x, [0 1; 0 2; 0 3]);
x = []; x(:, :) = [1 2 3];
asserts.isequal(x, [1 2 3]);
x = []; x(2, :) = [1 2 3];
asserts.isequal(x, [0 0 0; 1 2 3]);
%=============================================================================
% an empty right-hand side other than [] is an assignment, not a deletion
x = zeros(1, 0); x(1, :) = zeros(1, 0);
asserts.isequal(size(x), [1 0]);
x = zeros(1, 0); x(:, :) = zeros(1, 0);
asserts.isequal(size(x), [1 0]);
x = zeros(0, 3); x(:, 1) = zeros(1, 0);
asserts.isequal(size(x), [0 3]);
x = zeros(2, 0); x(:, :) = zeros(1, 0);
asserts.isequal(size(x), [2 0]);
x = magic(3); x(false(1, 3), 1) = zeros(0, 1);
asserts.isequal(x, magic(3));
assert_checkerror('x = magic(3); x(1, :) = zeros(1, 0);', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('x = magic(3); x(:, 1) = zeros(1, 0);', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
%=============================================================================
%=============================================================================
% a selection without elements only accepts a scalar or an empty value
x = zeros(2, 3); x(1, []) = 1;
asserts.isequal(x, zeros(2, 3));
x = zeros(2, 3); x([]) = 3;
asserts.isequal(x, zeros(2, 3));
x = zeros(2, 3); x(1, []) = zeros(1, 0);
asserts.isequal(x, zeros(2, 3));
x = zeros(2, 3); x([]) = zeros(0, 1);
asserts.isequal(x, zeros(2, 3));
assert_checkerror('x = zeros(2, 3); x(1, []) = [1 2];', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('x = zeros(2, 3); x(:, []) = ones(2, 2);', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('x = zeros(2, 3); x([]) = [1 2];', _('Size mismatch in assignment A(I) = B.'));
assert_checkerror('x = zeros(2, 3); x(false(2, 3)) = [1 2];', _('Size mismatch in assignment A(I) = B.'));
assert_checkerror('x = ones(2, 3, 2); x(:, :, []) = zeros(2, 3);', _('Size mismatch in assignment A(I1,I2,...,In) = B.'));
assert_checkerror('x = []; x(:) = [1 2 3];', _('Size mismatch in assignment A(I) = B.'));
