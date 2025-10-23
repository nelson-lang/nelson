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
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'php', 'Timeout', 30);
filename = fullfile(tempdir(), 'test_websave_args_5.json');
M = [1 2 3; 4 5 6];

% retry loop to make the test more robust
MAX_ATTEMPTS = 5;
PAUSE_SECS = [0, 1, 2, 4, 8];
fullname = '';
lastErrorMsg = '';
success = false;

for attempt = 1:MAX_ATTEMPTS
  % remove any stale file
  if isfile(filename)
    try delete(filename); catch; end
  end

  try
    % use HTTPS to avoid redirects
    fullname = websave(filename, 'https://httpbin.org/get', 'r', M, o);

    if isfile(fullname)
      info = dir(fullname);
      if info.bytes >= 20
        success = true;
        break;
      else
        lastErrorMsg = sprintf('Downloaded file too small (%d bytes).', info.bytes);
      end
    else
      lastErrorMsg = 'websave did not create the file.';
    end
  catch ex
    fullname = '';
    lastErrorMsg = ex.message;
  end

  pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
end

if ~success
  skip_testsuite(true, ['Could not obtain test data: ' lastErrorMsg]);
end

assert_istrue(isfile(fullname));
R = jsondecode(fileread(fullname))
%=============================================================================
