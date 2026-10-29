%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
global callbackSource;
callbackSource = [];
f = figure('Visible', 'off');
u = uicontrol('Parent', f, 'Style', 'pushbutton', 'CreateFcn', @storeCallbackSource);
asserts.isequal(callbackSource, u);
clear global callbackSource;
close(f);
%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
