%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('isequalto()', _('Wrong number of input arguments.'));
assert_checkerror('isequalto(1)', _('Wrong number of input arguments.'));
assert_checkerror('isequalto([1, 1])', _('Wrong number of input arguments.'));
%=============================================================================
assert_isequal(nargin('isequalto'), -1);
assert_isequal(nargout('isequalto'), 1);
%=============================================================================
assert_istrue(isequalto(1, 1));
assert_isfalse(isequalto(1, single(1)));
assert_istrue(isequal(1, single(1)));
%=============================================================================
