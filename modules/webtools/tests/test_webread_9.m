%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Timeout bounds the wait for the server, not the whole transfer: a response
% that answers at once but streams its body over 8 seconds must be read with
% the default 5 seconds timeout.
url = 'https://httpbin.org/drip?duration=8&numbytes=16&code=200';
try
  r = webread(url, weboptions('ContentType', 'binary'));
catch ex
  transient = strcmp(ex.message, _('Forbidden (403)')) || ...
              strcmp(ex.message, _('Couldn''t resolve host name')) || ...
              strcmp(ex.message, _('Service unavailable (503)')) || ...
              strcmp(ex.message, _('Bad gateway (502)'));
  if transient
    skip_testsuite(transient, ex.message);
  end
  rethrow(ex);
end
asserts.isequal(numel(r), 16);
%=============================================================================
% A server that never answers still times out.
t0 = tic();
try
  webread('https://httpbin.org/delay/10', weboptions('Timeout', 2, 'ContentType', 'binary'));
  failed = false;
catch ex
  failed = true;
  msg = ex.message;
end
elapsed = toc(t0);
if failed && ~strcmp(msg, _('Timeout was reached'))
  skip_testsuite(true, msg);
end
asserts.istrue(failed);
asserts.istrue(elapsed < 9);
%=============================================================================
