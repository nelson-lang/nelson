%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--AUDIO INPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
myVoice = audiorecorder();
tic(); recordblocking(myVoice, 5); elapsedTime = toc();
assert_istrue(elapsedTime <= 7);
assert_istrue(elapsedTime >= 5);
%=============================================================================
playblocking(myVoice);
%=============================================================================