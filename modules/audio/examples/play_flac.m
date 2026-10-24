%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
flac_audio = [modulepath('audio'), '/examples/kaneda.flac'];
devices = audiodevinfo();
if isempty(devices.output)
  error('This example requires an audio output device.');
end
[y, fs] = audioread(flac_audio);
playObj = audioplayer(y, fs);
playblocking(playObj);
delete(playObj)
clear playObj
%=============================================================================
