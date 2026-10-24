%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nflow_engine ships the C++ simulation/routing/codegen engine
% (nlsNflow_engine) and the headless code-generation builtin.
addgateway(modulepath('nflow_engine', 'builtin'), 'nflow_engine');
addpath(modulepath('nflow_engine', 'functions'), '-frozen');
%=============================================================================
