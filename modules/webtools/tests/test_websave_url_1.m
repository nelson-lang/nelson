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
o = weboptions('RequestMethod', 'get', 'Timeout', 30);
filename = fullfile(tempdir(), 'test_websave_args_1.json');

% retry loop to make the test more robust
MAX_ATTEMPTS = 5;
PAUSE_SECS = [0, 1, 2, 4, 8];
fullname = '';
lastErrorMsg = '';

while (~ok && cnt < MAX_ATTEMPTS)
  % ensure no stale file remains
  if isfile(filename)
    try delete(filename); catch; end
  end

  try
    % use HTTPS to avoid redirects; preserve original argument intent
    fullname = websave(filename, 'https://httpbin.org/get', 'r', i, "b+", 3, o);
    ok = isfile(fullname);
    if ok
      % basic content sanity check
      txt = fileread(fullname);
      if isempty(txt)
        ok = false;
        lastErrorMsg = 'Empty response from server.';
      end
    end
  catch ex
    fullname = '';
    lastErrorMsg = ex.message;
    ok = false;
  end

  cnt = cnt + 1;
  if ~ok
    pause(PAUSE_SECS(min(cnt, numel(PAUSE_SECS))));
  end
end

if ~ok
  skip_testsuite(true, ['Could not obtain test data: ' lastErrorMsg]);
end

assert_istrue(isfile(fullname));
R = jsondecode(fileread(fullname));
assert_istrue(isstruct(R.args));
% accept either order of fields, but ensure both expected fields exist and have expected values
fn = fieldnames(R.args);
assert_istrue(ismember('b_', fn) && ismember('r', fn));
assert_isequal(R.args.b_, '3');
assert_isequal(R.args.r, '0+1i');
%=============================================================================