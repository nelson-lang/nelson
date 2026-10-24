%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('struct'), 1);
asserts.isequal(nargout('struct'), 1);
%=============================================================================
A = struct();
asserts.isequal(class(A), 'struct');
asserts.isequal(size(A), [1 1]);
c = struct2cell(A);
asserts.isequal(class(c), 'cell');
asserts.isequal(size(c), [0 1]);
f = fieldnames(A);
asserts.isequal(class(f), 'cell');
asserts.isequal(size(f), [0 1]);
R = cell2struct(c, f);
asserts.isequal(class(R), 'struct');
asserts.isequal(size(R), [1 1]);
%=============================================================================
A = struct([]);
asserts.isequal(class(A), 'struct');
asserts.isequal(size(A), [0 0]);
c = struct2cell(A);
asserts.isequal(size(c), [0 0 0]);
f = fieldnames(A);
asserts.isequal(size(f), [0 1]);
R = cell2struct(c, f);
asserts.isequal(class(R), 'struct');
asserts.isequal(size(R), [0 0]);
%=============================================================================
A = struct(ones(1, 0));
asserts.isequal(class(A), 'struct');
asserts.isequal(size(A), [1 0]);
c = struct2cell(A);
asserts.isequal(class(c), 'cell');
asserts.isequal(size(c), [0 1 0]);
f = fieldnames(A);
asserts.isequal(size(f), [0 1]);
R = cell2struct(c, f);
asserts.isequal(class(R), 'struct');
asserts.isequal(size(R), [1 0]);
%=============================================================================
A = struct(ones(0, 1));
asserts.isequal(class(A), 'struct');
asserts.isequal(size(A), [0 1]);
c = struct2cell(A);
asserts.isequal(class(c), 'cell');
asserts.isequal(size(c), [0 0]);
f = fieldnames(A);
asserts.isequal(size(f), [0 1]);
R = cell2struct(c, f);
asserts.isequal(class(R), 'struct');
asserts.isequal(size(R), [0 1]);
%=============================================================================
if ~isempty(which('weboptions'))
  obj = weboptions();
  asserts.isequal(class(obj), 'weboptions');
  asserts.isequal(class(struct(obj)), 'struct');
end
%=============================================================================
R1 = struct('A', 1);
R2 = struct("A", 1);
asserts.isequal(R1, R2);
%=============================================================================
S = repmat(struct('id', 0, 'score', 0), 1, 4);
for k = 1:4
  S(k).id = k;
  S(k).score = k * 2;
end
asserts.isequal([S.id], 1:4);
asserts.isequal([S.score], 2:2:8);
%=============================================================================
S = struct('count', 0, 'last', 0);
for k = 1:8
  S.count = S.count + 1;
  S.last = k;
end
asserts.isequal(S.count, 8);
asserts.isequal(S.last, 8);
%=============================================================================
S = struct('meta', struct('count', 0, 'last', 0));
for k = 1:8
  S.meta.count = S.meta.count + 1;
  S.meta.last = k;
end
asserts.isequal(S.meta.count, 8);
asserts.isequal(S.meta.last, 8);
%=============================================================================
S = struct('b', 2, 'a', 1);
asserts.istrue(isstruct(S));
EmptyStruct = struct();
asserts.isequal(size(fieldnames(EmptyStruct)), [0 1]);
EmptyStructArray = struct([]);
asserts.isequal(size(EmptyStructArray), [0 0]);
%=============================================================================
% a string array value is one value, not a cell of values
S = struct('a', ["x", "y"]);
asserts.isequal(size(S), [1 1]);
asserts.isequal(S.a, ["x", "y"]);
%=============================================================================
