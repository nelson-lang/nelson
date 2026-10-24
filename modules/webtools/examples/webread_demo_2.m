%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Use a function_handle with weboptions
% to read an audio and play it directly
devices = audiodevinfo();
if isempty(devices.output)
  error('This example requires an audio output device.');
end
current = mfilename('fullpathext');
p = fileparts(current);
previousPath = addpath(p);
cleanupPath = onCleanup(@() path(previousPath));
fh = str2func('download_play');
o = weboptions('ContentType', 'binary', 'ContentReader', fh);
o.Timeout = 10;
try
  playerObj = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', o);
catch exception
  error(['This example requires network access to the public audio resource: ', exception.message]);
end
playblocking(playerObj);
delete(playerObj);
clear playerObj cleanupPath;
%=============================================================================
