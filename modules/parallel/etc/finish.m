%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
delete(FevalFuture_used());
delete(FevalQueue_used());
delete(backgroundPool_used());
%=============================================================================
% rmpath(modulepath(nelsonroot(), 'parallel', 'functions'));
removegateway(modulepath(nelsonroot(), 'parallel', 'builtin'));
%=============================================================================
