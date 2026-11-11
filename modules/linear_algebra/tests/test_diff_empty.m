%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% diff along a dimension of length 0 leaves the empty array as it is (it used
% to fail with "Index exceeds array bounds").
%=============================================================================
asserts.isequal(diff(zeros(0, 3)), zeros(0, 3));
asserts.isequal(diff(zeros(0, 3, 2)), zeros(0, 3, 2));
asserts.isequal(diff(zeros(3, 0, 5), 3), zeros(0, 0, 5));
asserts.isequal(diff(int8(zeros(0, 3))), int8(zeros(0, 3)));
asserts.isequal(diff(single(zeros(0, 3))), single(zeros(0, 3)));
asserts.isequal(diff(false(0, 3)), zeros(0, 3));
asserts.isequal(diff(char(zeros(0, 3))), zeros(0, 3));
asserts.isequal(diff(sparse(zeros(0, 3))), sparse(zeros(0, 3)));
asserts.istrue(issparse(diff(sparse(zeros(0, 3)))));
%=============================================================================
% already handled, kept as they are
asserts.isequal(diff([]), []);
asserts.isequal(diff(zeros(3, 0)), zeros(2, 0));
asserts.isequal(diff(zeros(0, 3), 1, 1), zeros(0, 3));
asserts.isequal(diff(zeros(0, 3), 1, 2), zeros(0, 2));
asserts.isequal(diff(zeros(3, 0), 1, 2), zeros(3, 0));
asserts.isequal(diff(zeros(2, 0, 3), 1, 1), zeros(1, 0, 3));
asserts.isequal(diff(single(zeros(3, 0))), single(zeros(2, 0)));
%=============================================================================
% an empty result keeps the class a subtraction would give
asserts.isequal(diff(int8(zeros(0, 3)), 1, 1), int8(zeros(0, 3)));
asserts.isequal(diff(uint16([1 2]), 5), uint16(zeros(1, 0)));
asserts.isequal(diff(false(0, 3), 1, 1), zeros(0, 3));
asserts.isequal(diff(int8([1 2 4])), int8([1 2]));
%=============================================================================
% an empty difference of a sparse array stays sparse, real and double
R = diff(sparse([1 2 3]), 5);
asserts.isequal(size(R), [1 0]);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
R = diff(sparse(5));
asserts.isequal(size(R), [0 0]);
asserts.istrue(issparse(R));
R = diff(sparse(true(1, 3)), 5);
asserts.isequal(size(R), [1 0]);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
R = diff(sparse(true));
asserts.isequal(size(R), [0 0]);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
R = diff(sparse(ones(2, 3)), 2);
asserts.isequal(size(R), [0 3]);
asserts.istrue(issparse(R));
R = diff(sparse(ones(2, 3)), 3, 2);
asserts.isequal(size(R), [2 0]);
asserts.istrue(issparse(R));
R = diff(sparse(ones(2, 3)), 5, 1);
asserts.isequal(size(R), [0 3]);
asserts.istrue(issparse(R));
R = diff(sparse(1i * [1 2]), 3);
asserts.isequal(size(R), [1 0]);
asserts.istrue(issparse(R));
asserts.istrue(isreal(R));
R = diff(sparse([]));
asserts.isequal(size(R), [0 0]);
asserts.istrue(issparse(R));
% the empty sparse result can be used in later operations
asserts.isequal(size(R - R), [0 0]);
asserts.isequal(size(diff(sparse(true(0, 3)))), [0 3]);
asserts.istrue(issparse(diff(sparse(true(0, 3)))));
% a sparse array has no third working dimension
asserts.checkerror('diff(sparse(ones(2, 3)), 1, 3)', ...
  _('Sparse arrays only allow a difference along dimension 1 or 2.'));
%=============================================================================
