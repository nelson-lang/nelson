%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'post');
o.Timeout = 60;
filename = [tempdir(), 'test_websave_post.json'];
fullname = websave(filename, 'http://httpbin.org/post', o);
R = jsondecode(fileread(fullname));
assert_isequal(R.url, 'http://httpbin.org/post');
%=============================================================================
filename = [tempdir(), 'test_websave_post.json'];
o = weboptions();
o.Timeout = 60;
cmd = 'fullname = websave(filename, ''http://httpbin.org/post'', o);';
assert_checkerror(cmd, _('Method Not Allowed (405)'));
%=============================================================================
