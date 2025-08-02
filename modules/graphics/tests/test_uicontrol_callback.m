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
%=============================================================================
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
f = figure();
h = uicontrol('Style', 'pushbutton', 'String', 'Click Me!', 'Position', [100, 100, 100, 50]);
h.Callback = @uiinspect;
h.DeleteFcn = @uiinspect;
%=============================================================================
