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
  o = weboptions('ContentType', 'audio');
  o.Timeout = 60;
  [y, fs] = webread('https://freewavesamples.com/files/Ensoniq-ZR-76-01-Dope-77.wav', o);
  assert_isequal(size(y), [137095           2]);
  assert_isapprox(y(1), -0.00854492, 1e-6);
  assert_isequal(fs, 44100);
end
%=============================================================================
