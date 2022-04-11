%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
delete(QObject_used())
rmpath(modulepath(nelsonroot(), 'qml_engine', 'functions'));
removegateway(modulepath(nelsonroot(), 'qml_engine', 'builtin'));
%=============================================================================
