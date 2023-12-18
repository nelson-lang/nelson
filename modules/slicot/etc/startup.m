%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('slicot', 'builtin'), 'slicot');
if ~SLICOTWrapper('load')
  warning('Nelson:slicot:loadingFails', _('library slicot not loaded.'))
  removemodule('slicot');
end
%=============================================================================
