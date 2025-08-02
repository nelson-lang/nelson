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
[y, fs] = audioread(flac_audio);
playObj = audioplayer(y, fs);
i = 0;
while (i < 10)
  playblocking(playObj, [10000, 140000]),
  i = i + 1
end
delete(playObj)
clear playObj
%=============================================================================
