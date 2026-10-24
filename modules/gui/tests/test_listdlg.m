%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
asserts.isequal(nargin('listdlg'), -1);
asserts.isequal(nargout('listdlg'), 2);
%=============================================================================
asserts.checkerror('listdlg()', sprintf(_('''%s'' expected.'), 'ListString'));
asserts.checkerror('listdlg(''Name'', ''Only title'')', sprintf(_('''%s'' expected.'), 'ListString'));
%=============================================================================
