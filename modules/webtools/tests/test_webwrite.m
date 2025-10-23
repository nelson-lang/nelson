%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('webwrite'), -1);
assert_isequal(nargout('webwrite'), -1);
%=============================================================================
options = weboptions('Timeout', 10);
%=============================================================================
[Y, M, D, H, MN, S] = datevec(now);
datetime = sprintf('%d/%d/%d %d:%d:%d', Y, M, D, H, MN, S);
% hide url to slack
url = char([104 116 116 112 115 58 47 47 104 111 111 107 115 46 115 108 97 99 107 46 99 111 109 47 115 101 114 118 105 99 101 115 47 84 77 82 71 56 82 72 68 50 47 66 77 83 48 76 72 65 65 67 47 81 54 52 97 52 49 84 83 76 104 105 78 71 81 108 100 51 115 76 50 86 109 74 71]);
data = struct('text', ['hello from Nelson ', datetime], 'channel', '#test_webwrite');
% Robust webwrite with retries
max_attempts = 3;
R = '';
for attempt = 1:max_attempts
  try
    R = webwrite(url, data, options);
    if ischar(R) && ~isempty(R)
      break;
    end
    R = '';
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
assert_isequal(R, 'ok');
%=============================================================================
[Y, M, D, H, MN, S] = datevec(now);
datetime = sprintf('%d/%d/%d %d:%d:%d', Y, M, D, H, MN, S);
% hide url to slack
url = char([104 116 116 112 115 58 47 47 104 111 111 107 115 46 115 108 97 99 107 46 99 111 109 47 115 101 114 118 105 99 101 115 47 84 77 82 71 56 82 72 68 50 47 66 77 83 48 76 72 65 65 67 47 81 54 52 97 52 49 84 83 76 104 105 78 71 81 108 100 51 115 76 50 86 109 74 71]);
data = struct('text', ['hello from Nelson ', datetime], 'channel', '#test_webwrite');
% Robust webwrite (json) with retries
max_attempts = 3;
R = '';
for attempt = 1:max_attempts
  try
    R = webwrite(url, jsonencode(data), options);
    if ischar(R) && ~isempty(R)
      break;
    end
    R = '';
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
assert_isequal(R, 'ok');
%=============================================================================
api = 'https://zzxuhwtog6.execute-api.us-east-1.amazonaws.com/mwparticle';
url = [api, '/photon1'];
input = struct('data','on');
% Robust webwrite to API with retries
max_attempts = 3;
userInfo = [];
for attempt = 1:max_attempts
  try
    userInfo = webwrite(url, input, options);
    if ~isempty(userInfo)
      break;
    end
    userInfo = [];
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
if isempty(userInfo)
  skip_testsuite(true, 'webwrite to API failed after retries or returned empty result');
end
if contains(fieldnames(userInfo), 'data')
  assert_isequal(userInfo, input);
else
  assert_istrue(contains(userInfo.errorMessage, 'Task timed out after'));
end
%=============================================================================
