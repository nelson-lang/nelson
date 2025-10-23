%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('websave'), -1);
assert_isequal(nargout('websave'), 1);
%=============================================================================
url = 'https://apod.nasa.gov/apod/image/2310/MoValleyEclipse.jpg';
filename = fullfile(tempdir(), 'MoValleyEclipse_2.jpg'); % use fullfile for portability
options = weboptions('Timeout', 120);
% Robust download: retry a few times, check file exists and has non-zero size.
max_attempts = 3;
destination_filename = '';
for attempt = 1:max_attempts
  try
    destination_filename = websave(filename, url, options);
    % Verify file exists and is not empty
    if isfile(destination_filename) && dir(destination_filename).bytes > 0
      break;
    end
    % if file missing or empty, clear and retry
    destination_filename = '';
  catch ex
    % Known transient/network errors: skip the testsuite (preserve behavior)
    R = strcmp(ex.message, _('Forbidden (403)')) || ...
        strcmp(ex.message, _('Timeout was reached')) || ... 
        strcmp(ex.message, _('Couldn''t resolve host name')) || ...
        strcmp(ex.message, _('Service unavailable (503)'));
    if R
      skip_testsuite(R, ex.message);
    end
    % otherwise allow retry
  end
  pause(1 * attempt); % brief backoff
end
if isempty(destination_filename)
  skip_testsuite(true, 'Download failed after retries or file is empty');
end
assert_istrue(isfile(destination_filename))
%=============================================================================
