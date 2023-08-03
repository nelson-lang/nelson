%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'delete');
filename = [tempdir(), 'test_websave_delete.json'];
fullname = websave(filename, 'http://jsonplaceholder.typicode.com/posts/1', o);
R = jsondecode(fileread(fullname));
REF = struct();
assert_isequal(R, REF);
%=============================================================================
filename = [tempdir(), 'test_websave_delete.json'];
o = weboptions('RequestMethod', 'delete');
o.Timeout = 120;
cmd = 'fullname = websave(filename, ''https://jsonplaceholder.typicode.com/posts?userId=1'', o);';
assert_checkerror(cmd, _('Not Found (404)'));
%=============================================================================
