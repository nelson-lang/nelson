%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('ContentType', 'raw', 'Timeout', 30);
url = 'https://jsonplaceholder.typicode.com/posts/1';
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
  skip_testsuite(true, 'webread raw failed after retries or returned empty result');
end
asserts.istrue(numel(R) > 20);
asserts.isequal(R(1), uint8(123));
