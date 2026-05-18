%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
S(1).name = 'alpha';
S(2).name = 'beta';
S(3).name = 'gamma';
fieldname = 'name';
assert_isequal({S.name}, {'alpha', 'beta', 'gamma'});
assert_isequal({S.(fieldname)}, {'alpha', 'beta', 'gamma'});
assert_isequal(unique_if_exists_for_bytecode_test(S, fieldname), {'alpha', 'beta', 'gamma'});
%=============================================================================
T(1).value = 10;
T(2).value = 20;
T(3).value = 30;
fieldname = 'value';
assert_isequal([T.value], [10 20 30]);
assert_isequal([T.(fieldname)], [10 20 30]);
%=============================================================================
A(1).payload = {1, 2, 3};
A(2).payload = {4, 5, 6};
fieldname = 'payload';
assert_isequal(A(2).(fieldname){2}, 5);
C = {A.(fieldname)};
assert_isequal(C{1}, {1, 2, 3});
assert_isequal(C{2}, {4, 5, 6});
%=============================================================================
D.values = [1 2 3 4 5];
fieldname = 'values';
assert_isequal(D.(fieldname)(2:end-1), [2 3 4]);
D.(fieldname)(2:end-1) = [];
assert_isequal(D.values, [1 5]);
%=============================================================================
for loopValue = 1:5
  clear loopValue;
  assert_isequal(exist('loopValue', 'var'), 0);
end
assert_isequal(exist('loopValue', 'var'), 0);
%=============================================================================
identity = @(x) x;
s = 0;
for k = 1:20
  s = s + identity(k);
end
assert_isequal(s, 210);
sumHandle = @vm_semantic_parity_sum;
assert_isequal(sumHandle(3, 4, 5), 12);
%=============================================================================
[n, values, first] = bytecode_varargin_varargout(42, 'kept', [1 2]);
assert_isequal(n, 3);
assert_isequal(values{2}, 'kept');
assert_isequal(first, 42);
[~, values, ~] = bytecode_varargin_varargout('discarded');
assert_isequal(values, {'discarded'});
%=============================================================================
contextValues = vm_semantic_parity_caller_context();
assert_isequal(contextValues, [15 22 0]);
%=============================================================================
tocText = evalc('tic(); toc();');
assert_istrue(contains(tocText, 'Elapsed time is'));
%=============================================================================
tocText = evalc('tic; toc;');
assert_istrue(contains(tocText, 'Elapsed time is'));
%=============================================================================
