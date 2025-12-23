%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--AUDIO INPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
recObj = audiorecorder();
recObj.StartFcn = @(src, event) disp('Recording started');
recObj.StopFcn = @(src, event) disp('Recording stopped');
recObj.TimerFcn = @(src, event) disp('Recording in progress');
record(recObj, 5);
%=============================================================================
