%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ok = false;
cnt = 0;
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'php');
o.Timeout = 30;
filename = [tempdir(), 'test_websave_args_5.json'];
M = [1 2 3; 4 5 6];
try
    fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
catch ex
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
R = jsondecode(fileread(fullname))
%=============================================================================
