%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://apod.nasa.gov/apod/image/2310/MoValleyEclipse.jpg';
filename = fullfile(tempdir(), 'MoValleyEclipse_1.jpg');
max_attempts = 3;
outfilename = '';
for attempt = 1:max_attempts
  try
    outfilename = websave(filename, url);
    if isfile(outfilename) && dir(outfilename).bytes > 0
      break;
    end
    outfilename = '';
  catch ex
    transient = strcmp(ex.message, _('Forbidden (403)')) || ...
                 strcmp(ex.message, _('Timeout was reached')) || ...
                 strcmp(ex.message, _('Couldn''t resolve host name')) || ...
                 strcmp(ex.message, _('Service unavailable (503)'));
    if transient
      skip_testsuite(transient, ex.message);
    end
    % otherwise allow retry
  end
  pause(0.5 * attempt);
end
if isempty(outfilename)
  skip_testsuite(true, 'websave failed after retries or returned empty file');
end
assert_istrue(isfile(outfilename));
%=============================================================================
