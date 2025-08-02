%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('struct'), 1);
assert_isequal(nargout('struct'), 1);
%=============================================================================
A = struct();
assert_isequal(class(A), 'struct');
assert_isequal(size(A), [1 1]);
c = struct2cell(A);
assert_isequal(class(c), 'cell');
assert_isequal(size(c), [0 1]);
f = fieldnames(A);
assert_isequal(class(f), 'cell');
assert_isequal(size(f), [0 1]);
R = cell2struct(c, f);
assert_isequal(class(R), 'struct');
assert_isequal(size(R), [1 1]);
%=============================================================================
A = struct([]);
assert_isequal(class(A), 'struct');
assert_isequal(size(A), [0 0]);
c = struct2cell(A);
assert_isequal(size(c), [0 0 0]);
f = fieldnames(A);
assert_isequal(size(f), [0 1]);
R = cell2struct(c, f);
assert_isequal(class(R), 'struct');
assert_isequal(size(R), [0 0]);
%=============================================================================
A = struct(ones(1, 0));
assert_isequal(class(A), 'struct');
assert_isequal(size(A), [1 0]);
c = struct2cell(A);
assert_isequal(class(c), 'cell');
assert_isequal(size(c), [0 1 0]);
f = fieldnames(A);
assert_isequal(size(f), [0 1]);
R = cell2struct(c, f);
assert_isequal(class(R), 'struct');
assert_isequal(size(R), [1 0]);
%=============================================================================
A = struct(ones(0, 1));
assert_isequal(class(A), 'struct');
assert_isequal(size(A), [0 1 ]);
c = struct2cell(A);
assert_isequal(class(c), 'cell');
assert_isequal(size(c), [0 0]);
f = fieldnames(A);
assert_isequal(size(f), [0 1]);
R = cell2struct(c, f);
assert_isequal(class(R), 'struct');
assert_isequal(size(R), [1 1]);
%=============================================================================
obj = weboptions();
assert_isequal(class(obj), 'weboptions');
assert_isequal(class(struct(obj)), 'struct');
%=============================================================================
R1 = struct('A', 1);
R2 = struct("A", 1);
assert_isequal(R1, R2);
%=============================================================================
