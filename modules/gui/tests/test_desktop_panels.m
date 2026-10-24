%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
filebrowser();
filebrowser('visible', false);
filebrowser('visible', true);
filebrowser('visible', 'toggle');
filebrowser('visible', true);
filebrowser('sync');
%=============================================================================
workspace();
workspace('visible', false);
workspace('visible', true);
workspace('visible', 'toggle');
workspace('visible', true);
workspace('sync');
%=============================================================================
commandhistory();
commandhistory('visible', true);
commandhistory('visible', 'toggle');
commandhistory('visible', true);
commandhistory('sync');
commandhistory('visible', false);
%=============================================================================
desktopPanelVariable = reshape(1:9, 3, 3);
openvar('desktopPanelVariable');
openvar('desktopPanelVariable');
%=============================================================================
terminal.closeAll();
desktopPanelTerminal = terminal('Name', 'Nelson desktop panel terminal test');
asserts.istrue(isa(desktopPanelTerminal, 'terminal'));
asserts.isequal(desktopPanelTerminal.WindowStyle, 'docked');
desktopPanelTerminal.focus();
activeTerminals = terminal.list();
asserts.istrue(numel(activeTerminals) >= 1);
delete(desktopPanelTerminal);
terminal.closeAll();
asserts.istrue(isempty(terminal.list()));
%=============================================================================
profile('clear');
profile('on');
for desktopPanelIndex = 1:3
  sin(rand(8, 8));
end
profile('off');
desktopPanelProfileInfo = profile('info');
asserts.istrue(isstruct(desktopPanelProfileInfo));
profile('show');
profile('show', 'nfl', 3);
profile('clear');
%=============================================================================
