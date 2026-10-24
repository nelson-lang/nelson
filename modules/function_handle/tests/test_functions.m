%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a named function handle is 'simple'
s = functions(@sin);
asserts.isequal(fieldnames(s), {'function'; 'type'; 'file'});
asserts.isequal(s.function, 'sin');
asserts.isequal(s.type, 'simple');
%=============================================================================
s = functions(str2func('cos'));
asserts.isequal(s.function, 'cos');
asserts.isequal(s.type, 'simple');
%=============================================================================
% an anonymous function is 'anonymous' and carries a workspace cell
f = @(x) x + 1;
s = functions(f);
asserts.isequal(fieldnames(s), {'function'; 'type'; 'file'; 'within_file_path'; 'workspace'});
asserts.isequal(s.type, 'anonymous');
asserts.isequal(s.function, func2str(f));
asserts.istrue(iscell(s.workspace));
asserts.isequal(size(s.workspace), [1 1]);
%=============================================================================
% captured variables are exposed in the workspace struct
a = 10;
b = 'hello';
g = @(x) x + a;
s = functions(g);
asserts.isequal(s.workspace{1}.a, 10);
%=============================================================================
h = @(x)[x, a, b];
s = functions(h);
asserts.isequal(s.workspace{1}.a, 10);
asserts.isequal(s.workspace{1}.b, 'hello');
%=============================================================================
% the workspace is always a 1x1 cell holding a struct
s = functions(@(x) x);
asserts.isequal(s.type, 'anonymous');
asserts.istrue(isstruct(s.workspace{1}));
%=============================================================================
% a non function handle errors
assert_checkerror('functions(3)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'function handle'));
%=============================================================================
