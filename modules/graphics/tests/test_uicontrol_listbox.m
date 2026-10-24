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
h = uicontrol('Style', 'listbox', 'String', {'Item 1', 'Item 2', 'Item 3'}, 'Position', [100, 100, 100, 100]);
asserts.isequal(h.Value, 1);
asserts.isequal(h.String, {'Item 1'; 'Item 2'; 'Item 3'});
drawnow();
pause(0.1);
h.Value = 2;
asserts.isequal(h.Value, 2);
drawnow();
pause(0.1);
h.Value = 3;
asserts.isequal(h.Value, 3);
drawnow();
pause(0.1);
%=============================================================================
