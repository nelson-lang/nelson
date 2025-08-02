
%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% read an distant audio file
options = weboptions();
options.ContentType = 'audio';
[y, fs] = webread('https://ccrma.stanford.edu/~jos/wav/Latin.wav', options);
playerObj = audioplayer(y, fs);
play(playerObj)
playerObj
%=============================================================================
