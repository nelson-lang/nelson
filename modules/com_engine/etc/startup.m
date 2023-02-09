%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  addgateway(modulepath('com_engine', 'builtin'), 'com_engine');
  addpath(modulepath('com_engine', 'functions'), '-frozen');
else
  removemodule('com_engine');
end
%=============================================================================
