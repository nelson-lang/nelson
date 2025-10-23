%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = fullfile(tempdir(), 'sunspots_annual.txt');
% Use HTTPS to avoid redirects and add retries
url = 'https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-indices/sunspot-numbers/american/lists/list_aavso-arssn_yearly.txt';
options = weboptions('Timeout', 120);

MAX_ATTEMPTS = 3;
PAUSE_SECS = [0, 5, 10];
destination_filename = '';
lastErrorMsg = '';
for attempt = 1:MAX_ATTEMPTS
  % remove any stale file before trying
  if isfile(filename)
    try
      delete(filename);
    catch
      % ignore deletion errors and continue
    end
  end

  try
    destination_filename = websave(filename, url, options);
    % Minimal sanity checks
    if ~isfile(destination_filename)
      lastErrorMsg = 'Downloaded file not found after websave.';
      pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
      continue;
    end
    info = dir(destination_filename);
    if info.bytes < 1000
      lastErrorMsg = sprintf('Downloaded file too small (%d bytes).', info.bytes);
      pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
      continue;
    end
    txt = fileread(destination_filename);
    if ~contains(txt, 'American', 'IgnoreCase', true)
      lastErrorMsg = 'Downloaded content did not contain expected marker.';
      pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
      continue;
    end
    % success
    break;
  catch ex
    % record last error and retry
    lastErrorMsg = ex.message;
    pause(PAUSE_SECS(min(attempt, numel(PAUSE_SECS))));
  end
end

% If still not available after retries, skip the test with reason
if isempty(destination_filename) || ~isfile(destination_filename)
  skip_testsuite(true, ['Could not obtain test data: ' lastErrorMsg]);
end

assert_istrue(isfile(destination_filename));
info = dir(destination_filename);
assert_istrue(info.bytes > 1000);
txt = fileread(destination_filename);
assert_istrue(contains(txt, 'American', 'IgnoreCase', true));
%=============================================================================
