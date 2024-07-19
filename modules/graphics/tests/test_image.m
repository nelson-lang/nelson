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
C = [0 20 40 60; 80 100 120 140; 160 180 200 220];
IM = image(C);
P =  properties(IM);
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
assert_isequal(P, REF);
%=============================================================================
assert_isequal(IM.Type, 'image');
assert_isequal(IM.AlphaData, 1);
assert_isequal(IM.CDataMapping, 'direct');
assert_isequal(IM.CData, C);
assert_isequal(IM.Tag, '');
assert_isequal(IM.UserData, []);
assert_isequal(IM.Visible, 'on');
assert_isequal(IM.XData, [1 4]);
assert_isequal(IM.YData, [1 3]);
%=============================================================================
