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
f = figure();
C = [0 20 40 60; 80 100 120 140; 160 180 200 220];
IM = image(C);
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
asserts.isequal(IM.CDataMapping, 'direct');
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
f = figure('Visible', 'off');
IM = image();
ax = gca();
asserts.isequal(size(IM.CData), [64 64 3]);
asserts.isequal(class(IM.CData), 'uint8');
asserts.isequal(IM.CDataMapping, 'direct');
asserts.isequal(IM.XData, [1 64]);
asserts.isequal(IM.YData, [1 64]);
asserts.isequal(ax.XLim, [0 70]);
asserts.isequal(ax.YLim, [0 70]);
asserts.isequal(ax.YDir, 'normal');
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 120 120]);
ax = axes('Position', [0 0 1 1]);
colormap([1 0 0; 0 1 0; 0 0 1; 1 1 0]);
image([1 2; 3 4]);
axis off;
frame = getframe(ax).cdata;
rows = size(frame, 1);
cols = size(frame, 2);
asserts.isequal(double(squeeze(frame(round(rows * 0.25), round(cols * 0.25), :))'), [255 0 0]);
asserts.isequal(double(squeeze(frame(round(rows * 0.25), round(cols * 0.75), :))'), [0 255 0]);
asserts.isequal(double(squeeze(frame(round(rows * 0.75), round(cols * 0.25), :))'), [0 0 255]);
asserts.isequal(double(squeeze(frame(round(rows * 0.75), round(cols * 0.75), :))'), [255 255 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
IM = image([10 13], [20 22], C);
ax = gca();
asserts.isequal(IM.XData, [10 13]);
asserts.isequal(IM.YData, [20 22]);
asserts.isequal(ax.XLim, [9.5 13.5]);
asserts.isequal(ax.YLim, [19.5 22.5]);
asserts.isequal(ax.DataAspectRatioMode, 'auto');
asserts.isequal(ax.PlotBoxAspectRatioMode, 'auto');
close(f);
%=============================================================================
