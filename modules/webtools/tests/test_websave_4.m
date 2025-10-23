%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Use raw.githubusercontent.com to fetch the binary directly and add retries
url = 'https://raw.githubusercontent.com/nelson-lang/nelson-website/master/images/qml_demos.png';
filename = fullfile(tempdir(), 'qml_demos.png');
options = weboptions('Timeout', 30);

% retry logic
MAX_ATTEMPTS = 3;
PAUSE_SECS = [0, 5, 10];
destination_filename = '';
lastErrorMsg = '';
success = false;

for attempt = 1:MAX_ATTEMPTS
  % remove any stale file before trying
  if isfile(filename)
    try
      delete(filename);
    catch
      % ignore deletion errors
    end
  end

  try
    destination_filename = websave(filename, url, options);
    if isfile(destination_filename)
      info = dir(destination_filename);
      if info.bytes > 1000
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
info = dir(destination_filename);
assert_istrue(info.bytes > 0);
%=============================================================================
