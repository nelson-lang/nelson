%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = str2func('asserts.istrue');
f(true);
f = str2func('asserts.isfalse');
f(false);
f = str2func('asserts.checkerror');
f('cos', _('Not enough input arguments.'));
f = str2func('asserts.isequal');
f([1 2], [1 2]);
f = str2func('asserts.isapprox');
f(1, 1 + 1e-8, 0, 1e-7);
f = str2func('asserts.size');
f(ones(2, 3), [2 3]);
f = str2func('asserts.class');
f(single(1), 'single');
f = str2func('asserts.empty');
f([]);
f = str2func('asserts.notempty');
f(1);
f = str2func('asserts.alltrue');
f([true true]);
f = str2func('asserts.allfalse');
f([false false]);
f = str2func('asserts.contains');
f('Nelson language', 'language');
f = str2func('asserts.match');
f('abc123', '^abc[0-9]+$');
f = str2func('asserts.type');
f(single(1), {'double', 'single'});
f = str2func('asserts.scalar');
f(1);
f = str2func('asserts.vector');
f([1 2]);
f = str2func('asserts.matrix');
f(ones(2, 2));
f = str2func('asserts.real');
f([1 2]);
f = str2func('asserts.finite');
f([1 2]);
f = str2func('asserts.nonNan');
f([1 Inf]);
f = str2func('asserts.sameSize');
f(ones(2, 3), zeros(2, 3));
f = str2func('asserts.numel');
f(ones(2, 3), 6);
f = str2func('asserts.throws');
f('cos', _('Not enough input arguments.'));
f = str2func('asserts.warningFree');
f('1 + 1');
%=============================================================================
stBegin = warning();
warning('on');
warning('on', 'Nelson:asserts:test');
f = str2func('asserts.warning');
f('warning(''Nelson:asserts:test'', ''expected warning'');', 'expected warning');
warning(stBegin);
%=============================================================================
f = str2func('asserts.diff');
msg = f([1 2], [1 3]);
asserts.istrue(contains(msg, 'First different index'));
%=============================================================================
f = str2func('asserts.fail');
[res, msg] = f('forced failure');
asserts.isfalse(res);
asserts.isequal(msg, 'forced failure');
%=============================================================================
f = str2func('asserts.noError');
f('1 + 1');
f = str2func('asserts.notEqual');
f(1, 2);
f = str2func('asserts.notApprox');
f(1, 2, eps);
f = str2func('asserts.lessThan');
f([1 2], 3);
f = str2func('asserts.lessOrEqual');
f([1 2], [1 2]);
f = str2func('asserts.greaterThan');
f([2 3], 1);
f = str2func('asserts.greaterOrEqual');
f([2 3], [2 3]);
f = str2func('asserts.inRange');
f([1 2], 0, 3);
f = str2func('asserts.rowVector');
f([1 2]);
f = str2func('asserts.columnVector');
f([1; 2]);
f = str2func('asserts.squareMatrix');
f(ones(2, 2));
f = str2func('asserts.ndims');
f(ones(2, 3), 2);
f = str2func('asserts.length');
f(ones(2, 3), 3);
f = str2func('asserts.rows');
f(ones(2, 3), 2);
f = str2func('asserts.columns');
f(ones(2, 3), 3);
f = str2func('asserts.startsWith');
f('Nelson language', 'Nelson');
f = str2func('asserts.endsWith');
f('Nelson language', 'language');
f = str2func('asserts.containsAll');
f('Nelson language', {'Nelson', 'language'});
f = str2func('asserts.containsAny');
f('Nelson language', {'toolbox', 'Nelson'});
f = str2func('asserts.matchesAll');
f('abc123', {'^abc', '[0-9]+$'});
f = str2func('asserts.matchesAny');
f('abc123', {'^xyz', '[0-9]+$'});
%=============================================================================
S = struct('a', 1, 'b', 2);
f = str2func('asserts.hasField');
f(S, 'a');
f = str2func('asserts.hasFields');
f(S, {'a', 'b'});
f = str2func('asserts.fields');
f(S, {'b', 'a'});
f = str2func('asserts.satisfies');
f(1, 'isnumeric');
f(1, @(x) isscalar(x));
%=============================================================================
raised = false;
try
  f = str2func('asserts.istrue');
  f(false, 'direct method failure');
catch ME
  raised = true;
  asserts.isequal(ME.message, 'direct method failure');
end
asserts.istrue(raised);
%=============================================================================
S = struct();
S.f = str2func('@(x) x + 1');
asserts.isequal(S.f(41), 42);
S.f = str2func('deal');
[a, b] = S.f(1, 2);
asserts.isequal(a, 1);
asserts.isequal(b, 2);
%=============================================================================
shadow = struct();
shadow.istrue = str2func('@(x) x + 1');
assert = shadow;
asserts.isequal(assert.istrue(41), 42);
clear assert;
%=============================================================================
