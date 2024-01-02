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
while (~ok && cnt < 10)
  try
    fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
    ok = true;
  catch
    cnt = cnt + 1;
  end
end
R = jsondecode(fileread(fullname))
%=============================================================================
