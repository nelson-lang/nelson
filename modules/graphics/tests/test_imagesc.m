%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
reset(groot());
%=============================================================================
f = figure();
C = [0 20 40 60; 80 100 120 140; 160 180 200 220];
IM = imagesc(C);
P = properties(IM);
REF = {'AlphaData';
  'AlphaDataMapping';
  'BeingDeleted';
  'BusyAction';
  'CData';
  'CDataMapping';
  'Children';
  'CreateFcn';
  'DeleteFcn';
  'Interruptible';
  'Parent';
  'Tag';
  'Type';
  'UserData';
  'Visible';
  'XData';
  'YData'};
asserts.istrue(all(ismember(REF, P)));
%=============================================================================
asserts.isequal(IM.Type, 'image');
asserts.isequal(IM.AlphaData, 1);
asserts.isequal(IM.CDataMapping, 'scaled');
asserts.isequal(IM.CData, C);
asserts.isequal(IM.Tag, '');
asserts.isequal(IM.UserData, []);
asserts.isequal(char(IM.Visible), 'on');
asserts.isequal(IM.XData, [1 4]);
asserts.isequal(IM.YData, [1 3]);
ax = gca();
asserts.isequal(ax.XLim, [0.5 4.5]);
asserts.isequal(ax.YLim, [0.5 3.5]);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1075 740]);
x = linspace(-1, 1, 1024)' * ones(1, 1024);
y = x';
Z = exp(-(x .^ 2 + y .^ 2) / 0.4);
imagesc(Z);
map = [0 0 0.3;
    0 0 0.4;
    0 0 0.5;
    0 0 0.6;
    0 0 0.8;
    0 0 1.0];
colormap(map);
drawnow();
ax = gca();
asserts.isequal(ax.XLim, [0.5 1024.5]);
asserts.isequal(ax.YLim, [0.5 1024.5]);
asserts.isequal(ax.XTick, 100:100:1000);
asserts.isequal(ax.YTick, 100:100:1000);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
frame = getframe(f);
lastColor = frame.cdata(:, :, 1) == 0 & frame.cdata(:, :, 2) == 0 & frame.cdata(:, :, 3) == 255;
asserts.istrue(sum(lastColor(:)) > 1000);
close(f);
%=============================================================================
close all
%=============================================================================
