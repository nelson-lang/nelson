%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = magic(5);
S.type = '()';
S.subs = {1:2, ':'};
R = subsref(A, S);
REF = [17 24 1 8 15;
  23 5 7 14 16];
asserts.isequal(R, REF);
%=============================================================================
C = {"one", 2, 'three'};
S = [];
S.type = '{}';
S.subs = {[1 2]};
[R1, R2] = subsref(C, S);
asserts.isequal(R1, "one");
asserts.isequal(R2, 2);
%=============================================================================
A = struct('number', 10);
S = [];
S.type = '.';
S.subs = 'number';
R = subsref(A, S);
asserts.isequal(R, 10);
%=============================================================================
v = [-Inf; 0; 1; -Inf];
idx = find(isfinite(v(:)));
R = v(idx);
asserts.isequal(size(idx), [2 1]);
asserts.isequal(size(R), [2 1]);
asserts.isequal(R, [0; 1]);
%=============================================================================
idx = [2; 3];
R = v(idx);
asserts.isequal(size(R), [2 1]);
asserts.isequal(R, [0; 1]);
%=============================================================================
idx = [2 3];
R = v(idx);
asserts.isequal(size(R), [2 1]);
asserts.isequal(R, [0; 1]);
%=============================================================================
v = [-Inf, 0, 1, -Inf];
idx = [2; 3];
R = v(idx);
asserts.isequal(size(R), [1 2]);
asserts.isequal(R, [0 1]);
%=============================================================================
idx = [2 3];
R = v(idx);
asserts.isequal(size(R), [1 2]);
asserts.isequal(R, [0 1]);
%=============================================================================
idx = find(isfinite(v(:)));
R = v(idx);
asserts.isequal(size(idx), [2 1]);
asserts.isequal(size(R), [1 2]);
asserts.isequal(R, [0 1]);
%=============================================================================
