%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://jsonplaceholder.typicode.com/posts/1/comments';
filename = fullfile(tempdir(), 'test.txt');

% Use JSON content type with a finite timeout and add retries
options = weboptions('ContentType','json','Timeout', 30);
MAX_ATTEMPTS = 3;
PAUSE_SECS = [0, 2, 5];
destination_filename = '';
lastErrorMsg = '';
success = false;

for attempt = 1:MAX_ATTEMPTS
  % remove any stale file before trying
  if isfile(filename)
    try delete(filename); catch; end
  end

  try
    destination_filename = websave(filename, url, options);

    if isfile(destination_filename)
      info = dir(destination_filename);
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
    lastErrorMsg = ex.message;
  end

  pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
end

if ~success
  skip_testsuite(true, ['Could not obtain test data: ' lastErrorMsg]);
end

assert_istrue(isfile(destination_filename));
txt = fileread(destination_filename);
st = jsondecode(txt);
assert_isequal(lower(st(1).email), lower('Eliseo@gardner.biz'));
%=============================================================================
