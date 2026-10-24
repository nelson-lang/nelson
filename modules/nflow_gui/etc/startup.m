%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('nflow_gui', 'builtin'), 'nflow_gui');
if ismodule('webview')
  % NFlow models and scripts join the examples gallery (demo, web desktop, NFlow).
  __webview__('registerExamplesProvider', modulepath('nflow_gui', 'builtin'), 'nlsNFlowExampleGroupsJson');
end
addpath(modulepath('nflow_gui', 'functions'), '-frozen');
%=============================================================================
