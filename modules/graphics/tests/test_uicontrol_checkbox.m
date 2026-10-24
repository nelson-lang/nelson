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
figure();
h = uicontrol('Style', 'checkbox', 'String', 'Click Me!', 'Position', [100, 100, 100, 50]);
asserts.isequal(h.Value, 0);
drawnow();
pause(0.1);
h.Value = 1;
asserts.isequal(h.Value, 1);
drawnow();
pause(0.1);
h.Min = -1;
h.Max = 1;
h.Value = -1;
asserts.isequal(h.Value, -1);
drawnow();
pause(0.1);
%=============================================================================
