%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% The 'BeingDeleted' state read by the callbacks below is an on/off property:
% it is exposed as a nelson.lang.OnOffSwitchState whose char() form is the
% 'on'/'off' text the callbacks record. Lock that contract so the callbacks
% keep building their log strings through char().
f = figure('Visible', 'off');
asserts.isequal(class(f.BeingDeleted), 'nelson.lang.OnOffSwitchState');
asserts.isequal(char(f.BeingDeleted), 'off');
asserts.istrue(f.BeingDeleted == 'off');
asserts.isfalse(logical(f.BeingDeleted));
delete(f);
%=============================================================================
global figureCloseCallbackLog;
figureCloseCallbackLog = {};
f = figure('Visible', 'off', 'DeleteFcn', @recordFigureDelete);
R = close(f);
asserts.istrue(R);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = figure('Visible', 'off', 'CloseRequestFcn', @recordFigureClose, ...
  'DeleteFcn', @recordFigureDelete);
R = close(f);
asserts.isfalse(R);
asserts.istrue(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, {'close:off:Close'});
delete(f);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, ...
  {'close:off:Close', 'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = figure('Visible', 'off', 'DeleteFcn', @recordFigureDelete);
delete(f);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = uifigure('Visible', 'off', 'DeleteFcn', @recordFigureDelete);
R = close(f);
asserts.istrue(R);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = uifigure('Visible', 'off', 'CloseRequestFcn', @recordFigureClose, ...
  'DeleteFcn', @recordFigureDelete);
R = close(f);
asserts.isfalse(R);
asserts.istrue(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, {'close:off:Close'});
delete(f);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, ...
  {'close:off:Close', 'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = uifigure('Visible', 'off', 'CloseRequestFcn', 'closereq', ...
  'DeleteFcn', @recordFigureDelete);
R = close(f);
asserts.istrue(R);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = uifigure('Visible', 'off', 'CloseRequestFcn', @recordFigureCloseAndDelete, ...
  'DeleteFcn', @recordFigureDelete);
R = close(f);
asserts.istrue(R);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, ...
  {'close-delete:off:Close', 'delete:on:ObjectBeingDestroyed'});
%=============================================================================
figureCloseCallbackLog = {};
f = uifigure('Visible', 'off', 'CloseRequestFcn', @recordFigureCloseAndDelete, ...
  'DeleteFcn', @recordFigureDelete);
feval(f.CloseRequestFcn, f, []);
asserts.isfalse(isgraphics(f));
asserts.isequal(figureCloseCallbackLog, ...
  {'close-delete:off:direct', 'delete:on:ObjectBeingDestroyed'});
%=============================================================================
clear global figureCloseCallbackLog;
close all
%=============================================================================
function recordFigureClose(src, event)
  global figureCloseCallbackLog;
  figureCloseCallbackLog{end + 1} = ['close:' char(src.BeingDeleted) ':' event.EventName];
end
%=============================================================================
function recordFigureCloseAndDelete(src, event)
  global figureCloseCallbackLog;
  if isempty(event)
    eventName = 'direct';
  else
    eventName = event.EventName;
  end
  figureCloseCallbackLog{end + 1} = ['close-delete:' char(src.BeingDeleted) ':' eventName];
  delete(src);
end
%=============================================================================
function recordFigureDelete(src, event)
  global figureCloseCallbackLog;
  figureCloseCallbackLog{end + 1} = ['delete:' char(src.BeingDeleted) ':' event.EventName];
end
%=============================================================================
