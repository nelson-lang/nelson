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
skip_if_portable_graphics_backend('native uicontrol resize events are not available.');
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
close all
f = uifigure('Visible', 'on', ...
             'Position', [100, 100, 360, 300], ...
             'AutoResizeChildren', 'off', ...
             'Tag', 'initial');
labels = [];
for k = 1:160
  labels(k) = uilabel(f, 'Text', int2str(k), ...
                      'Position', [10, 10, 40, 10]);
end
f.SizeChangedFcn = @resizeControlsCallback;
f.Position = [100, 100, 560, 360];
drawnow();
pause(0.1);
drawnow();

asserts.isequal(f.Tag, 'sizechanged');
asserts.isequal(length(f.Children), 160);
asserts.istrue(labels(1).Position(3) > 40);
asserts.istrue(labels(160).Position(2) > labels(1).Position(2));
close all
%=============================================================================
