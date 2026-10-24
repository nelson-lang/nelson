
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Download and play a short public audio resource.
devices = audiodevinfo();
if isempty(devices.output)
  error('This example requires an audio output device.');
end
options = weboptions('Timeout', 15);
options.ContentType = 'audio';
try
  [y, fs] = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', options);
catch exception
  error(['This example requires network access to the public audio resource: ', exception.message]);
end
playerObj = audioplayer(y, fs);
playblocking(playerObj);
delete(playerObj);
clear playerObj;
%=============================================================================
