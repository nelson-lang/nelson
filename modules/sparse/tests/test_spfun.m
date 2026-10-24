%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = sparse([2 0 -3; 0 4 0]);
R = spfun(@(x) x .* 10, S);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [20 0 -30; 0 40 0]);
asserts.isequal(nnz(R), 3);
%=============================================================================
% a function mapping a nonzero to zero prunes the entry
R = spfun(@(x) x - x, S);
asserts.istrue(issparse(R));
asserts.isequal(nnz(R), 0);
%=============================================================================
% no nonzero elements
R = spfun(@sin, sparse(2, 2));
asserts.istrue(issparse(R));
asserts.isequal(size(R), [2 2]);
asserts.isequal(nnz(R), 0);
%=============================================================================
% a full input is treated as sparse
R = spfun(@(x) x + 1, [1 0; 0 2]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 0; 0 3]);
%=============================================================================
% zeros are never passed to the function
R = spfun(@(x) 1 ./ x, sparse([2 0; 0 4]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [0.5 0; 0 0.25]);
%=============================================================================
% complex values
C = sparse([1 + 1i 0; 0 2]);
R = spfun(@(x) x .* 2, C);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 + 2i 0; 0 4]);
%=============================================================================
asserts.checkerror('spfun(1, sparse(2, 2))', _('First argument must be a function handle.'));
%=============================================================================
asserts.checkerror('spfun(@sin)', _('Not enough input arguments.'));
%=============================================================================
