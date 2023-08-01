%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
%=============================================================================
if ismodule('audio')
  o = weboptions('ContentType', 'binary', 'ContentReader', str2func('audioread'));
  o.Timeout = 120;
  R = webread('https://freewavesamples.com/files/Ensoniq-ZR-76-01-Dope-77.wav', o);
  assert_isequal(size(R), [137095           2]);
  assert_isapprox(R(5), 0.1467, 1e-3);
end
%=============================================================================
