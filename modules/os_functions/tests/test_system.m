%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('system'), 1);
assert_isequal(nargout('system'), 2);
%=============================================================================
binpath = modulepath(nelsonroot,'core','bin');
nelson_exe = ['"', binpath, '/nelson-cli', '"'];
nelson_cmd = 'a=35;exit(a);';
cmd = [nelson_exe, ' --execute "', nelson_cmd, '"'];
[a, b] = system(cmd);
assert_isequal(a, 35);
%=============================================================================
[s, w] = system(["echo hello", "echo my", "echo world"]);
assert_isequal(s, [0 0 0]);
assert_isequal(size(w), [1 3]);
assert_istrue(isa(w, 'string'));
assert_istrue(startsWith(w{1}, 'hello'));
assert_istrue(startsWith(w{2}, 'my'));
assert_istrue(startsWith(w{3}, 'world'));
%=============================================================================
if ispc()
  tic();
  [s, w, d] = system(["PING -n 5 127.0.0.1>nul", "PING -n 7 127.0.0.1>nul", "PING -n 10 127.0.0.1>nul"]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 4s", "sleep 6s", "sleep 9s"]);
  t = toc();
end
if (maxNumCompThreads() > 3)
  assert_istrue(t >= 9 && t < 11)
else
  assert_istrue(t >= 9 && t < 14)
end
assert_istrue(d(1) >= 4000 && d(1) <= 4500)
assert_istrue(d(2) >= 6000 && d(2) <= 6500)
assert_istrue(d(3) >= 9000 && d(2) <= 9500)
assert_isequal(s, zeros(1, 3))
%=============================================================================
if ispc()
  tic();
  [s, w, d] = system(["PING -n 5 127.0.0.1>nul", "PING -n 7 127.0.0.1>nul", "PING -n 10 127.0.0.1>nul"], [1, 5, 3]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 4s", "sleep 6s", "sleep 9s"], [1, 5, 3]);
  t = toc();
end
assert_istrue(t >= 5 && t < 7)
assert_istrue(d(1) >= 1000 && d(1) <= 4500)
assert_istrue(d(2) >= 5000 && d(2) <= 6500)
assert_istrue(d(3) >= 3000 && d(2) <= 9500)
assert_isequal(s, [130, 130, 130])
assert_isequal(w, ["ABORTED", "ABORTED", "ABORTED"])
%=============================================================================
