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
% Non-regression: a multi-line edit uicontrol (Max - Min > 1) used to crash
% Nelson with an EXCEPTION_ACCESS_VIOLATION because the widget dispatch could
% not recognize the multi-line widget and cast a QTextEdit to a QLineEdit.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 300 200]);
%=============================================================================
% Creating the multi-line edit must not crash.
h = uicontrol('Parent', f, 'Style', 'edit', 'Max', 2, 'Min', 0, ...
  'String', {'line1', 'line2'}, 'Position', [20 20 200 100]);
asserts.isequal(class(h.String), 'cell');
asserts.isequal(h.String, {'line1'; 'line2'});
drawnow();
pause(0.1);
%=============================================================================
% Updating the String of a multi-line edit goes through the widget recreate
% path (same crashing dispatch): it must round-trip too.
h.String = {'a', 'b', 'c'};
asserts.isequal(h.String, {'a'; 'b'; 'c'});
drawnow();
pause(0.1);
%=============================================================================
% Flipping Max toggles between the single-line and multi-line widgets.
h.Max = 1;
drawnow();
pause(0.1);
h.Max = 3;
drawnow();
pause(0.1);
%=============================================================================
close(f);
%=============================================================================
