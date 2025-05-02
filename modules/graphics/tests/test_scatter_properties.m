%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
REF = {	'BeingDeleted';
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
'MarkerEdgeColor';
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
