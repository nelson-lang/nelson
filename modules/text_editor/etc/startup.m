%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addgateway(modulepath('text_editor', 'builtin'), 'text_editor');
addpath(modulepath('text_editor', 'functions'), '-frozen');
%=============================================================================
if any(strcmp(argv(),'--vscode'))
  editor('vscode_command', 'code');
end
%=============================================================================
