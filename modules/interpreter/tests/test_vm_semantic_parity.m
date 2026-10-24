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
asserts.isequal({S.name}, {'alpha', 'beta', 'gamma'});
asserts.isequal({S.(fieldname)}, {'alpha', 'beta', 'gamma'});
asserts.isequal(unique_if_exists_for_bytecode_test(S, fieldname), {'alpha', 'beta', 'gamma'});
%=============================================================================
U(1) = 7;
U(2) = 9;
asserts.isequal(U, [7 9]);
%=============================================================================
T(1).value = 10;
T(2).value = 20;
T(3).value = 30;
fieldname = 'value';
asserts.isequal([T.value], [10 20 30]);
asserts.isequal([T.(fieldname)], [10 20 30]);
%=============================================================================
A(1).payload = {1, 2, 3};
A(2).payload = {4, 5, 6};
fieldname = 'payload';
asserts.isequal(A(2).(fieldname){2}, 5);
C = {A.(fieldname)};
asserts.isequal(C{1}, {1, 2, 3});
asserts.isequal(C{2}, {4, 5, 6});
%=============================================================================
D.values = [1 2 3 4 5];
fieldname = 'values';
asserts.isequal(D.(fieldname)(2:end - 1), [2 3 4]);
D.(fieldname)(2:end - 1) = [];
asserts.isequal(D.values, [1 5]);
E.values = [1 2 3 4 5];
E.values(2:4) = [8 9 10];
asserts.isequal(E.values, [1 8 9 10 5]);
%=============================================================================
for loopValue = 1:5
  clear loopValue;
  asserts.isequal(exist('loopValue', 'var'), 0);
end
asserts.isequal(exist('loopValue', 'var'), 0);
%=============================================================================
identity = @(x) x;
s = 0;
for k = 1:20
  s = s + identity(k);
end
asserts.isequal(s, 210);
sumHandle = @vm_semantic_parity_sum;
asserts.isequal(sumHandle(3, 4, 5), 12);
%=============================================================================
[n, values, first] = bytecode_varargin_varargout(42, 'kept', [1 2]);
asserts.isequal(n, 3);
asserts.isequal(values{2}, 'kept');
asserts.isequal(first, 42);
[~, values, ~] = bytecode_varargin_varargout('discarded');
asserts.isequal(values, {'discarded'});
%=============================================================================
contextValues = vm_semantic_parity_caller_context();
asserts.isequal(contextValues, [15 22 0]);
%=============================================================================
tocText = evalc('tic(); toc();');
elapsedPrefix = _('Elapsed time is {0:.6f} seconds.');
bracePosition = strfind(elapsedPrefix, '{');
if ~isempty(bracePosition)
  elapsedPrefix = elapsedPrefix(1:bracePosition(1) - 1);
end
asserts.istrue(contains(tocText, elapsedPrefix));
%=============================================================================
tocText = evalc('tic; toc;');
asserts.istrue(contains(tocText, elapsedPrefix));
%=============================================================================
