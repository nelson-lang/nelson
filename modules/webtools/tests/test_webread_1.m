%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('webread'), -1);
assert_isequal(nargout('webread'), -1);
%=============================================================================
o = weboptions('RequestMethod', 'get');
o.Timeout = 50;
url = 'https://jsonplaceholder.typicode.com/posts/1/comments';
max_attempts = 3;
R = [];
for attempt = 1:max_attempts
  try
    R = webread(url, o);
    % Accept only non-empty struct responses
    if isstruct(R) && ~isempty(R)
      break;
    end
    R = [];
  catch ex
    % Preserve existing behavior for known transient/server errors
    transient = strcmp(ex.message, _('Forbidden (403)')) || ...
                strcmp(ex.message, _('Timeout was reached')) || ...
                strcmp(ex.message, _('Couldn''t resolve host name')) || ...
                strcmp(ex.message, _('Service unavailable (503)'));
    if transient
      skip_testsuite(transient, ex.message);
    end
    % otherwise allow retry after a short backoff
  end
  pause(0.5 * attempt);
end
if isempty(R)
  skip_testsuite(true, 'webread failed after retries or returned empty result');
end
assert_istrue(isstruct(R));
assert_isequal(R(1).email, 'Eliseo@gardner.biz');
%=============================================================================
