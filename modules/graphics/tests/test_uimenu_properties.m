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
m = uimenu(f);
asserts.isequal(m.Type, 'uimenu');
asserts.isequal(m.Parent, f);
asserts.istrue(isempty(m.MenuSelectedFcn));
asserts.isequal(m.Tooltip, '');
asserts.isequal(char(m.Visible), 'on');
asserts.isapprox(m.ForegroundColor, [33 33 33] / 255, 1e-12);
asserts.isequal(char(m.Enable), 'on');
asserts.isequal(char(m.Separator), 'off');
asserts.isequal(m.Accelerator, '');
asserts.isequal(char(m.Checked), 'off');
asserts.isequal(m.Text, '');
asserts.isequal(m.Position, 1);
asserts.istrue(isempty(m.Children));
asserts.isequal(char(m.HandleVisibility), 'on');
asserts.istrue(isempty(m.ButtonDownFcn));
asserts.isequal(m.BusyAction, 'queue');
asserts.isequal(char(m.BeingDeleted), 'off');
asserts.isequal(char(m.Interruptible), 'on');
asserts.istrue(isempty(m.CreateFcn));
asserts.istrue(isempty(m.DeleteFcn));
asserts.isequal(m.Tag, '');
asserts.istrue(isempty(m.UserData));
asserts.isequal(char(m.Clipping), 'on');
%=============================================================================
m.Text = '&File';
m.Tooltip = 'file menu';
m.Visible = 'off';
m.ForegroundColor = [0.8 0.1 0.2];
m.Enable = 'off';
m.Separator = 'on';
m.Accelerator = 'F';
m.Checked = 'on';
m.MenuSelectedFcn = 'menuSelectedTrace = 1;';
m.ButtonDownFcn = 'buttonTrace = 1;';
m.BusyAction = 'cancel';
m.Interruptible = 'off';
m.Tag = 'menu-tag';
m.UserData = struct('name', 'payload');
m.Clipping = 'off';
asserts.isequal(m.Text, '&File');
asserts.isequal(m.Tooltip, 'file menu');
asserts.isequal(char(m.Visible), 'off');
asserts.isequal(m.ForegroundColor, [0.8 0.1 0.2]);
asserts.isequal(char(m.Enable), 'off');
asserts.isequal(char(m.Separator), 'on');
asserts.isequal(m.Accelerator, 'F');
asserts.isequal(char(m.Checked), 'on');
asserts.isequal(m.MenuSelectedFcn, 'menuSelectedTrace = 1;');
asserts.isequal(m.ButtonDownFcn, 'buttonTrace = 1;');
asserts.isequal(m.BusyAction, 'cancel');
asserts.isequal(char(m.Interruptible), 'off');
asserts.isequal(m.Tag, 'menu-tag');
asserts.isequal(m.UserData.name, 'payload');
asserts.isequal(char(m.Clipping), 'off');
%=============================================================================
m.Visible = 'on';
m.Enable = 'on';
child1 = uimenu(m, 'Text', 'child-1');
child2 = uimenu(m, 'Text', 'child-2');
asserts.isequal(m.Children, [child2; child1]);
child1.Position = 3;
asserts.isequal(m.Children, [child1; child2]);
asserts.isequal(child1.Parent, m);
%=============================================================================
cm = uicontextmenu(f);
contextChild = uimenu(cm, 'Text', 'context item');
asserts.isequal(contextChild.Parent, cm);
asserts.isequal(cm.Children, contextChild);
contextChild.Parent = m;
asserts.isequal(contextChild.Parent, m);
asserts.istrue(isempty(cm.Children));
asserts.istrue(any(m.Children == contextChild));
%=============================================================================
cm2 = uicontextmenu(f);
m.ContextMenu = cm2;
asserts.isequal(m.ContextMenu, cm2);
failed = false;
try
  m.ContextMenu = child1;
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(m.ContextMenu, cm2);
%=============================================================================
m.HandleVisibility = 'off';
asserts.istrue(isempty(findobj(f, 'Type', 'uimenu', 'Tag', 'menu-tag')));
asserts.istrue(any(allchild(f) == m));
m.HandleVisibility = 'on';
asserts.istrue(any(findobj(f, 'Type', 'uimenu', 'Tag', 'menu-tag') == m));
%=============================================================================
global menuCreateSource menuDeleteSource menuDeleteState;
menuCreateSource = [];
menuDeleteSource = [];
menuDeleteState = '';
life = uimenu(f, 'CreateFcn', @storeMenuCreateSource, 'DeleteFcn', @storeMenuDeleteSource);
asserts.isequal(menuCreateSource, life);
delete(life);
asserts.isequal(menuDeleteSource, life);
asserts.isequal(char(menuDeleteState), 'on');
clear global menuCreateSource menuDeleteSource menuDeleteState;
%=============================================================================
failed = false;
try
  m.Type = 'other';
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(m.Type, 'uimenu');
%=============================================================================
delete(contextChild);
asserts.istrue(~any(m.Children == contextChild));
delete(m);
asserts.istrue(isempty(findobj(f, 'Type', 'uimenu', 'Tag', 'menu-tag')));
close all
%=============================================================================
function storeMenuCreateSource(src, evt)
  global menuCreateSource;
  menuCreateSource = src;
end
%=============================================================================
function storeMenuDeleteSource(src, evt)
  global menuDeleteSource menuDeleteState;
  menuDeleteSource = src;
  menuDeleteState = src.BeingDeleted;
end
%=============================================================================
