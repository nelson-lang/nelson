%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% isKey(M, key), M.isKey(key) and M.Count on a char-key map in native storage
% take a native shortcut: same results and errors as the class methods
%=============================================================================
M = containers.Map({'a', 'b'}, {1, 2});
asserts.isequal(isKey(M, 'a'), true);
asserts.isequal(isKey(M, 'zz'), false);
asserts.isequal(M.isKey('b'), true);
asserts.isequal(M.isKey('zz'), false);
asserts.isequal(class(isKey(M, 'a')), 'logical');
asserts.isequal(size(isKey(M, 'a')), [1 1]);
asserts.isequal(M.Count, 2);
asserts.isequal(class(M.Count), 'double');
%=============================================================================
% keys the shortcut leaves to the method
asserts.isequal(isKey(M, "a"), true);
asserts.isequal(isKey(M, {'a', 'zz'; 'b', 'q'}), [true false; true false]);
asserts.isequal(isKey(M, ''), false);
asserts.isequal(isKey(M, char(zeros(1, 0))), false);
asserts.isequal(isKey(M, ['a'; 'b']), false);
asserts.isequal(isKey(M, 1), false);
%=============================================================================
% the storage follows assignments and removals
M('c') = 3;
asserts.isequal(isKey(M, 'c'), true);
asserts.isequal(M.Count, 3);
remove(M, 'a');
asserts.isequal(isKey(M, 'a'), false);
asserts.isequal(M.isKey('a'), false);
asserts.isequal(M.Count, 2);
%=============================================================================
% maps without native storage
N = containers.Map('KeyType', 'double', 'ValueType', 'any');
asserts.isequal(isKey(N, 1), false);
asserts.isequal(N.Count, 0);
N(1) = 'one';
asserts.isequal(isKey(N, 1), true);
asserts.isequal(N.Count, 1);
%=============================================================================
E = containers.Map();
asserts.isequal(isKey(E, 'a'), false);
asserts.isequal(E.Count, 0);
%=============================================================================
% errors are unchanged
errorId = '';
try
  [x, y] = isKey(M, 'b');
catch ex
  errorId = ex.identifier;
end
asserts.isequal(errorId, 'Nelson:tooManyOutputs');
errorId = '';
try
  [x, y] = M.isKey('b');
catch ex
  errorId = ex.identifier;
end
asserts.isequal(errorId, 'Nelson:tooManyOutputs');
errorId = '';
try
  isKey(M, 'a', 'b');
catch ex
  errorId = ex.identifier;
end
asserts.isequal(errorId, 'Nelson:tooManyInputs');
%=============================================================================
D = containers.Map({'a'}, {1});
delete(D);
errorId = '';
try
  isKey(D, 'a');
catch ex
  errorId = ex.identifier;
end
asserts.isequal(errorId, 'Nelson:interpreter:invalidClassdefHandle');
errorId = '';
try
  n = D.Count;
catch ex
  errorId = ex.identifier;
end
asserts.isequal(errorId, 'Nelson:interpreter:invalidClassdefHandle');
%=============================================================================
