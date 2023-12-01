%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/8
% <-- Short Description -->
% test_nargin & test_nargout failed in Windows binary version
%=============================================================================
r = dir([nelsonroot(), '/modules/core/tests/*.m']);
assert_istrue(length(r) > 0);
