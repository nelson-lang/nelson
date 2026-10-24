%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('nmm_gui', 'builtin'), 'nmm_gui');
if ismodule('webview')
  % The web desktop hosts the package manager as a panel through this dispatcher.
  __webview__('registerWebTool', 'nmm', modulepath('nmm_gui', 'builtin'), 'nlsNmmGuiWebToolDispatch');
end
addpath(modulepath('nmm_gui', 'functions'), '-frozen');
%=============================================================================
