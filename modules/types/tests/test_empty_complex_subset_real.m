%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% selecting or deleting every element of a complex array gives a real array
x = [1i, 2i];
asserts.istrue(isreal(x([])));
asserts.istrue(isreal(x(1, [])));
asserts.istrue(isreal(x(:, [])));
asserts.istrue(isreal(x(1:0)));
asserts.istrue(isreal(x(false)));
asserts.istrue(isreal(x(zeros(1, 0))));
asserts.isfalse(isreal(x(1:2)));
%=============================================================================
z = complex([]);
asserts.istrue(isreal(z(:)));
asserts.isequal(size(z(:)), [0 1]);
asserts.istrue(isreal(z(:, :)));
asserts.istrue(isreal(z([])));
asserts.isfalse(isreal(z));
%=============================================================================
y = 1i;
y(1) = [];
asserts.istrue(isreal(y));
y = [1i, 2i];
y(:) = [];
asserts.istrue(isreal(y));
y = [1i; 2i];
y(:, 1) = [];
asserts.istrue(isreal(y));
asserts.isequal(size(y), [2 0]);
y = [1i, 2i];
y([1 2]) = [];
asserts.istrue(isreal(y));
asserts.isequal(size(y), [1 0]);
y = [1i, 2i; 3i, 4i];
y(:, [1 2]) = [];
asserts.istrue(isreal(y));
asserts.isequal(size(y), [2 0]);
y = [1i, 2i];
y(1) = [];
asserts.isfalse(isreal(y));
asserts.isequal(y, 2i);
%=============================================================================
% deleting every row or column of a sparse complex array
y = sparse([1i; 2i]);
y(:, 1) = [];
asserts.istrue(isreal(y));
asserts.istrue(issparse(y));
asserts.isequal(size(y), [2 0]);
y = sparse([1i, 2i]);
y([1 2]) = [];
asserts.istrue(isreal(y));
asserts.isequal(size(y), [1 0]);
y = sparse([1i, 2i]);
y(:) = [];
asserts.istrue(isreal(y));
%=============================================================================
