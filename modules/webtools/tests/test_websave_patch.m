%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'patch');
filename = [tempdir(), 'test_websave_patch.json'];
fullname = websave(filename, 'http://httpbin.org/patch', o);
R = jsondecode(fileread(fullname));
assert_isequal(R.url, 'http://httpbin.org/patch');
%=============================================================================
filename = [tempdir(), 'test_websave_patch.json'];
cmd = 'fullname = websave(filename, ''http://httpbin.org/patch'');';
assert_checkerror(cmd, _('Method Not Allowed (405)'));
%=============================================================================
