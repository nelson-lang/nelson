%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ok = false;
cnt = 0;
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'repeating');
o.Timeout = 30;
filename = [tempdir(), 'test_websave_args_4.json'];
M = [1 2 3; 4 5 6];
try
  fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
catch ex
  fullname = '';
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
assert_istrue(isfile(fullname));
R = jsondecode(fileread(fullname))
%=============================================================================
