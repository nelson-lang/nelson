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
asserts.isequal(nargin('inputdlg'), 5);
asserts.isequal(nargout('inputdlg'), 1);
%=============================================================================
asserts.checkerror('inputdlg()', _('Not enough input arguments.'));
asserts.checkerror('inputdlg(''a'', ''b'', 1, {''c''}, struct(), ''extra'')', _('Too many input arguments.'));
asserts.checkerror('inputdlg(1)', sprintf(_('''%s'' expected.'), 'Text'));
%=============================================================================
