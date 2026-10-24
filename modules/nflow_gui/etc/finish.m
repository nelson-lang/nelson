%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%rmpath(modulepath('nflow_gui', 'functions'));
if ismodule('webview')
  __webview__('unregisterExamplesProvider', modulepath('nflow_gui', 'builtin'));
end
removegateway(modulepath('nflow_gui', 'builtin'));
%=============================================================================