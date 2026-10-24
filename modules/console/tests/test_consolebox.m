%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('consolebox'), 1);
asserts.isequal(nargout('consolebox'), -1);
%=============================================================================
status = consolebox('status');
asserts.istrue(islogical(status));
asserts.istrue(isscalar(status));
%=============================================================================
status = consolebox(false);
asserts.istrue(islogical(status));
asserts.istrue(isscalar(status));
%=============================================================================
