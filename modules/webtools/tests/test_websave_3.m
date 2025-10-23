%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://s.w-x.co/staticmaps/WEB_Current_Weather_Map_1280x720.jpg?crop=16:9&width=800&format=pjpg&auto=webp&quality=60';
filename = fullfile(tempdir(), 'earth2.jpg');
if isfile(filename)
  try
    rmfile(filename);
  catch
    % ignore deletion errors
  end
end

% Use a reasonable timeout and retry a few times with backoff
options = weboptions('Timeout', 30);
MAX_ATTEMPTS = 4;
PAUSE_SECS = [0, 5, 10, 20];
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

    % basic validation: file exists and is not tiny
    if isfile(destination_filename)
      info = dir(destination_filename);
      if info.bytes >= 1000
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
assert_istrue(success)
%=============================================================================
