%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = sparse([0 2 0; 3 0 4]);

R = sum(S);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([3 2 4]));
asserts.isequal(nzmax(R), 3);

R = sum(S, 2);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([2; 7]));
asserts.isequal(nzmax(R), 2);

R = prod(S);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0 0 0]));
asserts.isequal(nzmax(R), 1);

R = any(S);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true true true])));

R = any(S, 2);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true; true])));

R = all(S);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false false])));
asserts.isequal(nzmax(R), 1);

R = all(S, 2);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false; false])));
asserts.isequal(nzmax(R), 1);

asserts.istrue(any(S, 'all'));
asserts.isfalse(all(S, 'all'));

LS = sparse(logical([true false true; false true false]));
LF = logical([false true true; true false false]);

R = LS & LF;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false true; false false false])));
asserts.isequal(nzmax(R), 3);

R = LF & LS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false true; false false false])));
asserts.isequal(nzmax(R), 3);

R = ~LS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; true false true])));
asserts.isequal(nzmax(R), 3);

R = S(2, :);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([3 0 4]));
asserts.isequal(nzmax(R), 3);
%=============================================================================
