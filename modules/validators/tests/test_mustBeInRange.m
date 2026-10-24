%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
asserts.isequal(nargin('mustBeInRange'), -3);
asserts.isequal(nargout('mustBeInRange'), 0);
%=============================================================================
mustBeInRange(3, 3, 3)
mustBeInRange(3, 3, 3, 'inclusive')
mustBeInRange(3, 2, 4, 'exclude-lower', 'exclude-upper')
%=============================================================================
asserts.checkerror('mustBeInRange(3,3,3,''exclusive'')', sprintf(_('%s must be in range.'), 'Value'), 'Nelson:validators:LeftOpenRightOpen');
%=============================================================================
