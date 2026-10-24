%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s.a = 1; s.b = 2; s.c = 3;
%=============================================================================
assert_isequal(structfun(@(x) x * 2, s), [2; 4; 6]);
%=============================================================================
r = structfun(@(x) x + 1, s, 'UniformOutput', false);
assert_isequal(r.a, 2);
assert_isequal(r.b, 3);
assert_isequal(r.c, 4);
%=============================================================================
assert_isequal(structfun(@(x) x > 1, s), [false; true; true]);
%=============================================================================
t.x = [1 2 3];
r2 = structfun(@sum, t);
assert_isequal(r2, 6);
%=============================================================================
asserts.checkerror('structfun(@sin, 3)', _('Second argument must be a scalar structure.'));
asserts.checkerror('structfun(@sin, 3)', _('Second argument must be a scalar structure.'), 'Nelson:data_structures:secondArgumentScalarStructure');
%=============================================================================
% 'ErrorHandler' and several outputs
R = structfun(@(x) error('boom'), struct('a', 1, 'b', 2), 'ErrorHandler', @(e, x) -1);
asserts.isequal(R, [-1; -1]);
[a, b] = structfun(@(x) deal(x, x * 2), struct('p', 1, 'q', 2));
asserts.isequal(a, [1; 2]);
asserts.isequal(b, [2; 4]);
[a, b] = structfun(@(x) deal(x, [x x]), struct('p', 1, 'q', 2), 'UniformOutput', false);
asserts.isequal(a, struct('p', 1, 'q', 2));
asserts.isequal(b, struct('p', [1 1], 'q', [2 2]));
%=============================================================================
