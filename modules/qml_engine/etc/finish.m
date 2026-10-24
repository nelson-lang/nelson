%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a lazy (never loaded) gateway cannot own live objects
[~, ~, qmlGatewayState] = gatewayinfo(modulepath('qml_engine', 'builtin'));
if strcmp(qmlGatewayState, 'loaded')
  delete(QObject_used())
end
rmpath(modulepath('qml_engine', 'functions'));
removegateway(modulepath('qml_engine', 'builtin'));
%=============================================================================
