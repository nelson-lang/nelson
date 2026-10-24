%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('isequalto()', _('Not enough input arguments.'));
asserts.checkerror('isequalto(1)', _('Not enough input arguments.'));
asserts.checkerror('isequalto([1, 1])', _('Not enough input arguments.'));
%=============================================================================
asserts.isequal(nargin('isequalto'), -1);
asserts.isequal(nargout('isequalto'), 1);
%=============================================================================
asserts.istrue(isequalto(1, 1));
asserts.isfalse(isequalto(1, single(1)));
asserts.istrue(isequal(1, single(1)));
%=============================================================================
