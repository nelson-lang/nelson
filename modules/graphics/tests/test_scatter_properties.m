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
f = figure();
s = scatter(1, 1);
assert_isequal(s.Type, 'scatter');
R = properties(s);
REF = { 'AlphaData';	
'BeingDeleted';
'BusyAction';
'CData';
'CDataMode';
'Children';
'CreateFcn';
'DeleteFcn';
'DisplayName';
'Interruptible';
'LineWidth';
'Marker';
'MarkerEdgeAlpha';
'MarkerEdgeColor';
'MarkerFaceAlpha';
'MarkerFaceColor';
'Parent';
'SizeData';
'Tag';
'Type';
'UserData';
'Visible';
'XData';
'XDataMode';
'YData';
'ZData'};
assert_isequal(R, REF);
%=============================================================================
