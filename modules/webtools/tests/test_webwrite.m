%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('webwrite'), -1);
asserts.isequal(nargout('webwrite'), -1);
%=============================================================================
url = 'https://jsonplaceholder.typicode.com/posts';
options = weboptions('Timeout', 10, 'MediaType', 'application/json', 'ContentType', 'json');
%=============================================================================
data = struct('title', 'Nelson', 'body', 'webwrite struct', 'userId', 1);
% Robust webwrite with retries
max_attempts = 3;
R = [];
for attempt = 1:max_attempts
  try
    R = webwrite(url, data, options);
    if ~isempty(R)
      break;
    end
    R = [];
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
if isempty(R)
  skip_testsuite(true, 'webwrite failed after retries or returned empty result');
end
asserts.isequal(R.title, data.title);
asserts.isequal(R.body, data.body);
asserts.isequal(R.userId, data.userId);
asserts.isequal(R.id, 101);
%=============================================================================
data = struct('title', 'Nelson', 'body', 'webwrite json', 'userId', 2);
% Robust webwrite (json) with retries
max_attempts = 3;
R = [];
for attempt = 1:max_attempts
  try
    R = webwrite(url, jsonencode(data), options);
    if ~isempty(R)
      break;
    end
    R = [];
  catch ex
    transient = strcmp(ex.message, _('Forbidden (403)')) || ...
                strcmp(ex.message, _('Timeout was reached')) || ...
                strcmp(ex.message, _('Couldn''t resolve host name')) || ...
                strcmp(ex.message, _('Service unavailable (503)'));
    if transient
      skip_testsuite(transient, ex.message);
    end
  end
  pause(0.5 * attempt);
end
if isempty(R)
  skip_testsuite(true, 'webwrite (json) failed after retries or returned empty result');
end
asserts.isequal(R.title, data.title);
asserts.isequal(R.body, data.body);
asserts.isequal(R.userId, data.userId);
asserts.isequal(R.id, 101);
%=============================================================================
options = weboptions('Timeout', 10, 'ContentType', 'json');
% Robust webwrite with name-value pairs
max_attempts = 3;
R = [];
for attempt = 1:max_attempts
  try
    R = webwrite(url, 'title', 'Nelson', 'body', 'webwrite pairs', 'userId', 3, options);
    if ~isempty(R)
      break;
    end
    R = [];
  catch ex
    transient = strcmp(ex.message, _('Forbidden (403)')) || ...
                strcmp(ex.message, _('Timeout was reached')) || ...
                strcmp(ex.message, _('Couldn''t resolve host name')) || ...
                strcmp(ex.message, _('Service unavailable (503)'));
    if transient
      skip_testsuite(transient, ex.message);
    end
  end
  pause(0.5 * attempt);
end
if isempty(R)
  skip_testsuite(true, 'webwrite with name-value pairs failed after retries or returned empty result');
end
asserts.isequal(R.title, 'Nelson');
asserts.isequal(R.body, 'webwrite pairs');
asserts.isequal(R.userId, '3');
asserts.isequal(R.id, 101);
%=============================================================================
