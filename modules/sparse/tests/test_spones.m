%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = [1,0;3,4];
R = spones(S);
assert_istrue(issparse(R));
[I, J, V] = IJV(R);
I_REF = [ 1;     2;     2];
assert_isequal(I, I_REF);
J_REF = [   1;     1;    2];
assert_isequal(J, J_REF);
V_REF = [1 ; 1; 1];
assert_isequal(V, V_REF);
%=============================================================================
S = sparse([1,0;3,4]);
R = spones(S);
assert_istrue(issparse(R));
[I, J, V] = IJV(R);
I_REF = [ 1;     2;     2];
assert_isequal(I, I_REF);
J_REF = [   1;     1;    2];
assert_isequal(J, J_REF);
V_REF = [1 ; 1; 1];
assert_isequal(V, V_REF);
%=============================================================================
