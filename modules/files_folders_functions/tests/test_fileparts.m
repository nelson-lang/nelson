%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[p, f, e] = fileparts('filename');
assert_isequal('', p);
assert_isequal('filename', f);
assert_isequal('', e);
%=============================================================================
[p1, f1, e1] = fileparts([nelsonroot(), '/etc/finish.m']);
p2 = fileparts([nelsonroot(), '/etc/finish.m'], 'path');
f2 = fileparts([nelsonroot(), '/etc/finish.m'], 'filename');
e2 = fileparts([nelsonroot(), '/etc/finish.m'], 'extension');
assert_isequal(p1, p2);
assert_isequal(f1, f2);
assert_isequal(e1, e2);
%=============================================================================
[p, f, e] = fileparts('filename.ext');
assert_isequal('', p);
assert_isequal('filename', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts('/filename.ext');
assert_isequal('/', p);
assert_isequal('filename', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts('dir/filename.ext');
assert_isequal('dir', p);
assert_isequal('filename', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts('./filename.ext');
assert_isequal('.', p);
assert_isequal('filename', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts ('dir1/dir2/filename.ext');
assert_isequal('dir1/dir2', p);
assert_isequal('filename', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts ('/dir1/dir2/filename.ext');
assert_isequal('/dir1/dir2', p);
assert_isequal('filename', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts ('./.ext');
assert_isequal('.', p);
assert_isequal('', f);
assert_isequal('.ext', e);
%=============================================================================
[p, f, e] = fileparts ('.ext');
assert_isequal('', p);
assert_isequal('', f);
assert_isequal('.ext', e);
%=============================================================================
[p,f,e] = fileparts('c:/Windows/');
assert_isequal('c:/Windows', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
[p,f,e] = fileparts('c:/Windows//');
assert_isequal('c:/Windows', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
[p,f,e] = fileparts('c://');
assert_isequal('c:/', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
[p,f,e] = fileparts('c:');
assert_isequal('c:', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
[p,f,e] = fileparts('c:/');
assert_isequal('c:/', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
