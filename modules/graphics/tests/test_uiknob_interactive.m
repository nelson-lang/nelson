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
clear('__uiknob_changing_value__', '__uiknob_changed_value__', '__uidiscreteknob_changed_value__');
f = uifigure('Name', 'uiknob drag test', 'Position', [100 100 380 260]);
kb = uiknob(f, 'Value', 25, 'Limits', [0 100], 'Position', [72 78 110 110]);
dk = uiknob(f, 'discrete', 'Position', [214 78 110 110]);
kb.ValueChangingFcn = @(src, event) assignin('base', '__uiknob_changing_value__', event.Value);
kb.ValueChangedFcn = @(src, event) assignin('base', '__uiknob_changed_value__', event.Value);
dk.ValueChangedFcn = @(src, event) assignin('base', '__uidiscreteknob_changed_value__', event.Value);
uilabel(f, 'Text', 'Drag both knobs (mouse). Then click a knob and use the', ...
  'Position', [40 36 300 22], 'HorizontalAlignment', 'center');
uilabel(f, 'Text', 'arrow keys: Up/Right raise, Down/Left lower (2.5 per step).', ...
  'Position', [40 18 300 22], 'HorizontalAlignment', 'center');
drawnow();
waitfor(f);
%=============================================================================
