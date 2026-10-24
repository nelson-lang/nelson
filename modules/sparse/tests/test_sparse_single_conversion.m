%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% double, single and logical keep a sparse array sparse and return the
% requested class (real and complex).
%=============================================================================
x = [single(1), sparse(1)];
R = double(x);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 1]);
%=============================================================================
m = sparse(single([1 0 2; 0 3 0]));
R = double(m);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 0 2; 0 3 0]);
asserts.isequal(nnz(R), 3);
R = double(sparse(single(zeros(2, 0))));
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [2 0]);
%=============================================================================
R = single(sparse(1));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single(1));
R = single(sparse([1 0 2]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1 0 2]));
R = single(sparse(true));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single(1));
R = single(m);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1 0 2; 0 3 0]));
R = single(sparse(zeros(0, 3)));
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(size(R), [0 3]);
%=============================================================================
R = logical(m);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [true false true; false true false]);
asserts.checkerror('logical(sparse(single([1 NaN])))', _('Conversion to logical with NaN is not possible.'));
%=============================================================================
c = sparse(single([1+2i 0 3; 0 0 -1i]));
R = double(c);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1+2i 0 3; 0 0 -1i]);
R = single(sparse([1i 0 2]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1i 0 2]));
R = single(c);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1+2i 0 3; 0 0 -1i]));
R = real(c);
asserts.istrue(issparse(R));
asserts.isequal(full(R), single([1 0 3; 0 0 0]));
%=============================================================================
