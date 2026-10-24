%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A package call written as a statement asks for no output, the way an
% unqualified call statement does. It used to ask for one, because a dotted
% chain was taken for an expression to display; a function that only acts when
% nargout is zero then never acted.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/package_function_call'];
addpath(path_test, '-begin');
clear functions
%=============================================================================
[bare, terminated, assigned, value] = packageNargoutHelper();
assert_isequal(bare, 0);
assert_isequal(terminated, 0);
assert_isequal(assigned, 1);
assert_isequal(value, 1);
%=============================================================================
% Asking for no output leaves no `ans`, and asking for one still displays it.
clear ans
lowerpkg.twice(21);
assert_isfalse(exist('ans', 'var') == 1);
%=============================================================================
% A returned value is still captured and displayed by a bare call statement.
clear ans
lowerpkg.twice(21)
assert_isequal(ans, 42);
%=============================================================================
% Indexing a field of a variable keeps displaying its value: only a call is
% lowered as a call.
clear ans
s.data = [10 20 30];
s.data(2)
assert_isequal(ans, 20);
clear ans
s.data
assert_isequal(ans, [10 20 30]);
%=============================================================================
rmpath(path_test);
%=============================================================================
