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
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
close all
%=============================================================================
asserts.istrue(isempty(gco()));
f = figure('Visible', 'off');
ax = axes('Parent', f);
% gco reports what the user clicked, so building an object does not make it
% current: see test_currentobject_construction.
asserts.istrue(isempty(gco()));
l = __line__('Parent', ax, 'XData', 1:3, 'YData', 1:3);
asserts.istrue(isempty(gco()));
f.CurrentObject = ax;
asserts.isequal(gco(), ax);
ax.HandleVisibility = 'off';
asserts.istrue(isempty(gco()));
ax.HandleVisibility = 'on';
f.CurrentObject = l;
asserts.isequal(gco(), l);
%=============================================================================
global callbackSource;
global callbackObject;
global callbackFigure;
global callbackFigureFromGcbf;
global callbackFigureType;
global callbackFigureFromGcbfType;
callbackSource = [];
callbackObject = [];
callbackFigure = [];
callbackFigureFromGcbf = [];
callbackFigureType = '';
callbackFigureFromGcbfType = '';
h = uicontrol('Parent', f, 'Style', 'pushbutton', 'CreateFcn', @storeCurrentCallbackState);
asserts.isequal(callbackSource, h);
asserts.isequal(callbackObject, h);
asserts.isequal(callbackFigure, f);
asserts.isequal(callbackFigureFromGcbf, f);
clear global callbackSource;
clear global callbackObject;
clear global callbackFigure;
clear global callbackFigureFromGcbf;
clear global callbackFigureType;
clear global callbackFigureFromGcbfType;
%=============================================================================
close all
%=============================================================================
