%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--ADV-CLI MODE-->
%=============================================================================
addpath([nelsonroot(),'/modules/graphics/tests/test_callback']);
f = figure('Position', [300, 300, 300, 200]);
f.KeyReleaseFcn=@uiinspect;
f.KeyPressFcn=@uiinspect;
f.ButtonDownFcn=@uiinspect;
