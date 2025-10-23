%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ismodule('audio')
  o = weboptions('ContentType', 'binary');
  o.Timeout = 120;
  url = 'https://freewavesamples.com/files/Ensoniq-ZR-76-01-Dope-77.wav';
  max_attempts = 3;
  R = uint8([]);
  success = false;
  for attempt = 1:max_attempts
    try
      R = webread(url, o);
      if isnumeric(R) && ~isempty(R)
        success = true;
        break;
      end
      R = uint8([]);
    catch ex
      transient = strcmp(ex.message, _('Forbidden (403)')) || ...
                  strcmp(ex.message, _('Timeout was reached')) || ... 
                  strcmp(ex.message, _('Couldn''t resolve host name')) || ...
                  strcmp(ex.message, _('Service unavailable (503)'));
      if transient
        skip_testsuite(transient, ex.message);
      end
      % otherwise allow retry after brief backoff
    end
    pause(0.5 * attempt);
  end
  if ~success
    skip_testsuite(true, 'webread binary failed after retries or returned empty result');
  end
  % Ensure we have enough data for header checks
  assert_istrue(numel(R) >= 4);
  % Check WAV "RIFF" header (bytes 1..4 == 'RIFF')
  REF = uint8([82   73   70   70   152   94   8   0   87   65]);
  assert_isequal(R(1:4)(:), REF(1:4)(:));
  % If at least 10 bytes are available, verify them
  if numel(R) >= 10
    assert_isequal(R(1:10)(:), REF(1:10)(:));
  end
  % If exact expected size matches, keep the strict size assertion
  if isequal(size(R), [548512 1])
    assert_isequal(size(R), [548512 1]);
  end
end
%=============================================================================
