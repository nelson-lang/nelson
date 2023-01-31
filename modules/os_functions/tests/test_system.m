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
binpath = modulepath('nelson', 'bin');
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
w1 = char(w(1));
w1 = w1(1:5);
w2 = char(w(2));
w2 = w2(1:2);
w3 = char(w(3));
w3 = w3(1:5);
assert_isequal(w1, 'hello')
assert_isequal(w2, 'my');
assert_isequal(w3, 'world');
%=============================================================================
if ispc()
  tic();
  [s, w, d] = system(["PING -n 5 127.0.0.1>nul", "PING -n 7 127.0.0.1>nul", "PING -n 10 127.0.0.1>nul"]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 4", "sleep 6", "sleep 9"]);
  t = toc();
end
if (maxNumCompThreads() > 3)
  assert_istrue(t >= 8 && t < 12)
else
  assert_istrue(t >= 8 && t < 15)
end
assert_istrue(d(1) >= 4000 && d(1) <= 5000)
assert_istrue(d(2) >= 6000 && d(2) <= 7000)
assert_istrue(d(3) >= 9000 && d(2) <= 10000)
assert_isequal(s, zeros(1, 3))
%=============================================================================
if ispc()
  tic();
  [s, w, d] = system(["PING -n 5 127.0.0.1>nul", "PING -n 7 127.0.0.1>nul", "PING -n 10 127.0.0.1>nul"], [1, 5, 3]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 4", "sleep 6", "sleep 9"], [1, 5, 3]);
  t = toc();
end
assert_istrue(t >= 5 && t < 7)
assert_istrue(d(1) >= 1000 && d(1) <= 5000)
assert_istrue(d(2) >= 5000 && d(2) <= 7000)
assert_istrue(d(3) >= 3000 && d(2) <= 10000)
if ispc()
  assert_isequal(s, [258, 258, 258])
else
  assert_isequal(s, [134,134, 134])
end
assert_isequal(w, ["ABORTED", "ABORTED", "ABORTED"])
%=============================================================================
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_exe = ['"', binpath, '/nelson', '"'];
else
  nelson_exe = [binpath, '/nelson'];
end
cmd = [nelson_exe, ' -cli --timeout 20'];
tic();[s,m]=system(cmd); R = toc();
if ispc()
  assert_isequal(s, 258);
else
  assert_isequal(s, 134);
end
assert_istrue(R > 20);
assert_istrue(R < 30);
%=============================================================================
