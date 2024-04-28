%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ismodule('audio')
  o = weboptions('ContentType', 'binary', 'ContentReader', str2func('audioread'));
  o.Timeout = 120;
  try
    R = webread('https://freewavesamples.com/files/Ensoniq-ZR-76-01-Dope-77.wav', o);
  catch ex
    R = strcmp(ex.message, _('Forbidden (403)')) || ...
        strcmp(ex.message, _('Timeout was reached')) || ... 
        strcmp(ex.message, _('Couldn''t resolve host name'));
    skip_testsuite(R, ex.message)
  end
end
%=============================================================================
