%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
delete(backgroundPool_used());
delete(FevalFuture_used());
delete(FevalQueue_used());
%=============================================================================
% rmpath(modulepath('parallel', 'functions'));
removegateway(modulepath('parallel', 'builtin'));
%=============================================================================
