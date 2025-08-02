%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ogg_audio = [modulepath('audio'), '/examples/drums.ogg'];
[y, fs] = audioread(ogg_audio);
Y = [y;y;y;y];
playObj = audioplayer(Y, fs);
playblocking(playObj)
delete(playObj)
clear playObj
%=============================================================================
