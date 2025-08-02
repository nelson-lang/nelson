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
% <--WITH DISPLAY-->
%=============================================================================
addpath([nelsonroot(), '/modules/qml_engine/tests/']);
h = errordlg();
q = h;
clear h;
assert_isfalse(isvalid(q));
assert_checkerror('acquirevar(''local'', ''h'')', _('variable not found.'));
%=============================================================================
