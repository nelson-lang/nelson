%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('ContentType', 'binary', 'ContentReader', str2func('fileread'), ...
  'FollowLocation', true, 'Timeout', 30);
url = 'https://jsonplaceholder.typicode.com/posts/1';
try
  R = webread(url, o);
catch ex
  transient = strcmp(ex.message, _('Forbidden (403)')) || ...
              strcmp(ex.message, _('Timeout was reached')) || ...
              strcmp(ex.message, _('Couldn''t resolve host name')) || ...
              strcmp(ex.message, _('Service unavailable (503)'));
  if transient
    skip_testsuite(transient, ex.message);
  end
  rethrow(ex);
end
asserts.istrue(contains(R, '"id": 1'));
asserts.istrue(contains(R, '"userId": 1'));
%=============================================================================
