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
  addgateway(modulepath(nelsonroot(), 'com_engine', 'builtin'));
  addpath(modulepath(nelsonroot(), 'com_engine', 'functions'), '-frozen');
else
  removemodule('com_engine');
end
%=============================================================================
