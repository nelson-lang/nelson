%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(symvar('x.^2'), {'x'});
%=============================================================================
assert_isequal(symvar('a + b*c'), {'a'; 'b'; 'c'});
%=============================================================================
% Result is sorted and unique.
assert_isequal(symvar('y + x + y'), {'x'; 'y'});
%=============================================================================
% A name written as a function call is not a variable; its arguments are.
assert_isequal(symvar('sin(x) + pi'), {'x'});
assert_isequal(symvar('max(a, b) + c'), {'a'; 'b'; 'c'});
assert_isequal(symvar('a(end) + b'), {'b'; 'end'});
%=============================================================================
% A bare (uncalled) name is kept, even if it is the name of a function.
assert_isequal(symvar('sin + x'), {'sin'; 'x'});
%=============================================================================
% Field access names are not variables.
assert_isequal(symvar('a.b + c'), {'a'; 'c'});
assert_isequal(symvar('foo.bar.baz + q'), {'foo'; 'q'});
%=============================================================================
% Language keywords are kept.
assert_isequal(symvar('for + z'), {'for'; 'z'});
assert_isequal(symvar('end + true + false'), {'end'; 'false'; 'true'});
%=============================================================================
% Special values are excluded.
assert_isequal(symvar('i + j + eps'), cell(0, 1));
assert_isequal(symvar('Inf + NaN + x'), {'x'});
assert_isequal(symvar('sin(pi)'), cell(0, 1));
%=============================================================================
% String scalar input is accepted.
assert_isequal(symvar("u + v"), {'u'; 'v'});
%=============================================================================
% Function handle input is accepted.
assert_isequal(symvar(@(x) x .^ 2), {'x'});
%=============================================================================
assert_checkerror('symvar(["a", "b"])', _('Input argument must be a character vector or a string scalar.'));
%=============================================================================
