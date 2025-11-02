%=============================================================================
% Copyright (c) 2017-2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/93
% <-- Short Description -->
% playblocking updated to manage range.
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
flac_audio = [modulepath('audio'), '/examples/kaneda.flac'];
[y, fs] = audioread(flac_audio);
playObj = audioplayer(y, fs);
playblocking(playObj, [10000, 140000]);
%=============================================================================
