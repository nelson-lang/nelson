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
o = weboptions('RequestMethod', 'get');
o.Timeout = 30;
filename = [tempdir(), 'test_websave_args_1.json'];
while (~ok && cnt < 10)
  try
    fullname = websave(filename, 'http://httpbin.org/get', 'r', i, "b+", 3, o);
    ok = true;
  catch
    fullname = '';
    cnt = cnt + 1;
  end
end
skip_testsuite(~ok, 'Timeout reached')
assert_istrue(isfile(fullname));
R = jsondecode(fileread(fullname));
assert_istrue(isstruct(R.args));
assert_isequal(fieldnames(R.args), {'b_'; 'r'});
assert_isequal(R.args.b_, '3');
assert_isequal(R.args.r, '0+1i');
%=============================================================================