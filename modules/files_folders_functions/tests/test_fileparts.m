%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
assert_isequal('c:/Windows/', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
[p,f,e] = fileparts('c://');
assert_isequal('c:/', p);
assert_isequal('', f);
assert_isequal('', e);
%=============================================================================
[p,f,e] = fileparts('c:');
if ispc()
  assert_isequal('c:', p);
  assert_isequal('', f);
  assert_isequal('', e);
else
  assert_isequal('', p);
  assert_isequal('c:', f);
  assert_isequal('', e);
end
%=============================================================================
[p,f,e] = fileparts('c:/');
if ispc()
  assert_isequal('c:/', p);
  assert_isequal('', f);
  assert_isequal('', e);
else
  assert_isequal('c:', p);
  assert_isequal('', f);
  assert_isequal('', e);
end
%=============================================================================
url = char([104 116 116 112 115 58 47 47 104 111 111 107 115 46 115 108 97 99 107 46 99 111 109 47 115 101 114 118 105 99 101 115 47 84 77 82 71 56 82 72 68 50 47 66 77 83 48 76 72 65 65 67 47 81 54 52 97 52 49 84 83 76 104 105 78 71 81 108 100 51 115 76 50 86 109 74 71]);
[p,f,e] = fileparts(url);
assert_isequal(f , 'Q64a41TSLhiNGQld3sL2VmJG');
assert_isequal(e , '');
assert_isequal(length(p) , 52);
%=============================================================================
