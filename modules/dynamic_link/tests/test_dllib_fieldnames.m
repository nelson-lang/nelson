%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('@dllib/isprop'), 2);
asserts.isequal(nargout('@dllib/isprop'), 1);
%=============================================================================
path_ref = modulepath('dynamic_link', 'builtin');
lib = dlopen(path_ref);
asserts.isequal(fieldnames(lib), {'Path'});
%=============================================================================
