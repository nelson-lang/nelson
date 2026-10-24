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
close all
%=============================================================================
f = figure('Visible', 'off');
cm = uicontextmenu(f);
asserts.isequal(cm.Type, 'uicontextmenu');
asserts.isequal(cm.Parent, f);
asserts.isequal(char(cm.HandleVisibility), 'on');
asserts.isequal(cm.BusyAction, 'queue');
asserts.isequal(char(cm.BeingDeleted), 'off');
asserts.isequal(char(cm.Interruptible), 'on');
asserts.isequal(char(cm.Clipping), 'on');
asserts.istrue(isempty(cm.Children));
asserts.istrue(any(allchild(f) == cm));
%=============================================================================
cm.Tag = 'ctx-tag';
cm.UserData = struct('value', 42);
asserts.isequal(cm.Tag, 'ctx-tag');
asserts.isequal(cm.UserData.value, 42);
%=============================================================================
m1 = uimenu(cm, 'Text', 'first');
m2 = uimenu(cm, 'Text', 'second');
asserts.isequal(cm.Children, [m2; m1]);
asserts.isequal(m1.Parent, cm);
asserts.isequal(m2.Parent, cm);
%=============================================================================
cm.HandleVisibility = 'off';
asserts.istrue(isempty(findobj(f, 'Type', 'uicontextmenu')));
asserts.istrue(any(allchild(f) == cm));
cm.HandleVisibility = 'on';
asserts.istrue(any(findobj(f, 'Type', 'uicontextmenu') == cm));
%=============================================================================
f2 = figure('Visible', 'off');
cm.Parent = f2;
asserts.isequal(cm.Parent, f2);
asserts.istrue(~any(allchild(f) == cm));
asserts.istrue(any(allchild(f2) == cm));
%=============================================================================
ax = axes('Parent', f2);
ax.ContextMenu = cm;
asserts.isequal(ax.ContextMenu, cm);
failed = false;
try
  ax.ContextMenu = ax;
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
global contextMenuCreateSource contextMenuDeleteSource;
contextMenuCreateSource = [];
contextMenuDeleteSource = [];
cm2 = uicontextmenu(f2, 'CreateFcn', @storeContextMenuCreateSource, ...
  'DeleteFcn', @storeContextMenuDeleteSource);
asserts.isequal(contextMenuCreateSource, cm2);
delete(cm2);
asserts.isequal(contextMenuDeleteSource, cm2);
clear global contextMenuCreateSource contextMenuDeleteSource;
%=============================================================================
failed = false;
try
  cm.Type = 'other';
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(cm.Type, 'uicontextmenu');
delete(cm);
asserts.istrue(isempty(findobj(f2, 'Type', 'uicontextmenu', 'Tag', 'ctx-tag')));
close all
%=============================================================================
function storeContextMenuCreateSource(src, evt)
  global contextMenuCreateSource;
  contextMenuCreateSource = src;
end
%=============================================================================
function storeContextMenuDeleteSource(src, evt)
  global contextMenuDeleteSource;
  contextMenuDeleteSource = src;
end
%=============================================================================
