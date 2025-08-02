%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--GUI MODE-->
%=============================================================================
assert_isequal(nargin('errordlg'), 3);
assert_isequal(nargout('errordlg'), 1);
%=============================================================================
h = errordlg()
assert_isequal(class(h), 'QObject')
h = errordlg('errorstring')
assert_isequal(class(h), 'QObject')
h = errordlg('errorstring','dlgname')
assert_isequal(class(h), 'QObject')
h = errordlg('errorstring','dlgname','on')
assert_isequal(class(h), 'QObject')
%=============================================================================
